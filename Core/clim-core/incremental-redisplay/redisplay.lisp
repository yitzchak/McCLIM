;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) Copyright 2002 by Michael McDonald <mikemac@mikemac.com>
;;;  (c) Copyright 2002-2004 by Tim Moore <moore@bricoworks.com>
;;;  (c) Copyright 2014 by Robert Strandh <robert.strandh@gmail.com>
;;;  (c) Copyright 2021 by Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Incremental redisplay.
;;;

(in-package #:clim-internals)

(defun redisplay (record stream &key (check-overlapping t))
  (redisplay-output-record record stream check-overlapping))

;;; Take the specification at its word that the x/y and parent-x/parent-y
;;; arguments are "entirely bogus."
(defgeneric redisplay-output-record (record stream &optional check-overlapping)
  (:method ((record updating-output-record)
            (stream updating-output-stream-mixin)
            &optional (check-overlapping t))
    (let ((*current-updating-output* record)
          (current-graphics-state (medium-graphics-state stream)))
      (unwind-protect
           (progn
             (set-medium-cursor-position (start-graphics-state record) stream)
             (with-stream-redisplaying (stream)
               (compute-new-output-records record stream))
             (multiple-value-bind
                   (erases moves draws erase-overlapping move-overlapping)
                 (compute-difference-set record check-overlapping)
               (note-output-record-child-changed
                (output-record-parent record) record :change
                nil nil stream
                erases moves draws erase-overlapping move-overlapping
                :check-overlapping check-overlapping))
             (delete-stale-updating-output record))
        (set-medium-cursor-position current-graphics-state stream)))))

(defgeneric compute-new-output-records (record stream)
  (:method ((record standard-updating-output-record) stream)
    (with-output-recording-options (stream :record t :draw nil)
      (map-over-updating-output
       #'(lambda (r)
           (let ((sub-record (sub-record r)))
             (when sub-record
               (setf (old-children r) sub-record)
               (setf (output-record-dirty r) :updating)
               (setf (rectangle-edges* (old-bounds r))
                     (rectangle-edges* sub-record)))))
       record
       nil)
      (force-output stream)
      ;; Why is this binding here? We need the "environment" in this call that
      ;; computes the new records of an outer updating output record to resemble
      ;; that when a record's contents are computed in invoke-updating-output.
      (letf (((stream-current-output-record stream)
              (output-record-parent record)))
            (compute-new-output-records-1 record
                                          stream
                                          (output-record-displayer record))))))

;;; Create the sub-record that holds the new contents of the updating output
;;; record.
(defun %invoke-updating (record stream displayer)
  (letf (((stream-current-output-record stream) record))
    (with-new-output-record (stream)
      (funcall displayer stream))))

(defun compute-new-output-records-1 (record stream displayer)
  "Like compute-new-output-records with an explicit displayer function."
  (check-type record standard-updating-output-record)
  (when-let ((sub-record (sub-record record)))
    (delete-output-record sub-record record))
  ;; Don't add this record repeatedly to a parent updating-output-record.
  (unless (eq (output-record-parent record)
              (stream-current-output-record stream))
    (setf (output-record-parent record) nil)
    (add-output-record record (stream-current-output-record stream)))
  (reinitialize-instance record)
  (%invoke-updating record stream displayer)
  (setf (output-record-dirty record) :updated))

(defconstant +fixnum-bits+ (integer-length most-positive-fixnum))

(declaim (inline hash-coords))
(defun hash-coords (x1 y1 x2 y2)
  (declare (type coordinate x1 y1 x2 y2))
  (let ((hash-val 0))
      (declare (type fixnum hash-val))
      (labels ((rot4 (val)
                 (dpb (ldb (byte 4 0) val)
                      (byte 4 (- +fixnum-bits+ 4 1))
                      (ash val -4)))
               (mix-it-in (val)
               (let ((xval (sxhash val)))
                 (declare (type fixnum xval))
                 (when (minusp val)
                   (setq xval (rot4 xval)))
                 (setq hash-val (logxor (rot4 hash-val) xval)))))
        (declare (inline rot4 mix-it-in))
        (mix-it-in x1)
        (mix-it-in y1)
        (mix-it-in x2)
        (mix-it-in y2)
        hash-val)))

(defgeneric output-record-hash (record)
  (:documentation "Produce a value that can be used to hash the output record
in an equalp hash table")
  (:method  ((record standard-bounding-rectangle))
    (slot-value record 'coordinates))
  (:method ((record output-record))
    (with-bounding-rectangle* (x1 y1 x2 y2) record
      (hash-coords x1 y1 x2 y2))))

(defgeneric compute-difference-set (record &optional check-overlapping
                                             offset-x offset-y
                                             old-offset-x old-offset-y)
  (:method ((record standard-updating-output-record)
            &optional (check-overlapping t)
              offset-x offset-y
              old-offset-x old-offset-y)
    (declare (ignore offset-x offset-y old-offset-x old-offset-y)
             (values list list list list list))
    ;; (declare (values erases moves draws #|erase-overlapping move-overlapping|#))
    (let ((old-table (make-hash-table :test #'equalp))
          (new-table (make-hash-table :test #'equalp))
          (all-table (make-hash-table)))
      (collect (old-records new-records)
        (flet ((collect-1 (record set)
                 (setf (gethash record all-table) t)
                 (ecase set
                   (:old
                    (old-records record)
                    (push record (gethash (output-record-hash record) old-table)))
                   (:new
                    (new-records record)
                    (push record (gethash (output-record-hash record) new-table))))))
          (labels ((gather-records (record set)
                     (typecase record
                       (displayed-output-record
                        (collect-1 record set))
                       (updating-output-record
                        (ecase (output-record-dirty record)
                          ((:clean :moved)
                           (collect-1 record set))
                          ((:updating :updated)
                           (let ((sub (ecase set
                                        (:old (old-children record))
                                        (:new (sub-record record)))))
                             (map-over-output-records #'gather-records sub
                                                      nil nil set)))))
                       (otherwise
                        (map-over-output-records #'gather-records record
                                                 nil nil set)))))
            (gather-records record :old)
            (gather-records record :new)))
        (collect (erases moves draws)
          (flet ((add-record (rec)
                   (if (updating-output-record-p rec)
                       (ecase (output-record-dirty rec)
                         (:moved
                          ;; If we ever use the new position for something
                          ;; then the specification says it stick it here.
                          (moves (list rec (old-bounds rec) #|new-position|#)))
                         (:clean
                          ;; no need to redraw clean records.
                          nil)
                         #+ (or)
                         ((:updating :updated)
                          ;; UPDATING-OUTPUT-RECORDs with the state :UPDATED
                          ;; are not collected (their children are collected).
                          (error "Updated recoreds are not collected!")))
                       (flet ((match-record (r) (output-record-equal rec r)))
                         (let* ((hash (output-record-hash rec))
                                ;; The bounding rectangle is always the same.
                                (entry (list rec (bounding-rectangle rec)))
                                (old-p (some #'match-record (gethash hash old-table)))
                                (new-p (some #'match-record (gethash hash new-table))))
                           (cond ((null new-p) (erases entry))
                                 ((null old-p) (draws entry))
                                 ;; Record siblings might have been reordered
                                 ;; so we need to "move it" in place.
                                 (t (moves entry))))))))
            (alexandria:maphash-keys #'add-record all-table))
          (if (null check-overlapping)
              (values (erases) (moves) (draws)      nil     nil)
              (values      nil     nil (draws) (erases) (moves))))))))

;;; Suppress the got-sheet/lost-sheet notices during redisplay.

(defmethod note-output-record-lost-sheet :around
    (record (sheet updating-output-stream-mixin))
  (declare (ignore record))
  (unless (stream-redisplaying-p sheet)
    (call-next-method)))

(defmethod note-output-record-got-sheet :around
    (record (sheet updating-output-stream-mixin))
  (declare (ignore record))
  (unless (stream-redisplaying-p sheet)
    (call-next-method)))

(defun delete-stale-updating-output (record)
  (map-over-updating-output
   #'(lambda (r)
       (when (eq (output-record-dirty r) :updating)
         (delete-from-map (parent-cache r)
                          (output-record-unique-id r)
                          (output-record-id-test r))))
   record
   t))

(defgeneric propagate-output-record-changes-p
    (record child mode old-position old-bounding-rectangle)
  (:method (record child mode old-position old-bounding-rectangle)
    nil))

(defgeneric propagate-output-record-changes
    (record child mode
     &optional old-position old-bounding-rectangle erases moves draws
       erase-overlapping move-overlapping check-overlapping)
  (:method (record child mode
            &optional old-position old-bounding-rectangle erases moves draws
              erase-overlapping move-overlapping check-overlapping)
    (declare (ignore record child mode
                     old-position old-bounding-rectangle erases moves draws
                     erase-overlapping move-overlapping check-overlapping))
    (error "This is a stub!")))

(defgeneric note-output-record-child-changed
    (record child mode old-position old-bounding-rectangle stream
     &optional erases moves draws erase-overlapping move-overlapping
     &key check-overlapping)
  (:method (record child mode old-position old-bounding-rectangle stream
            &optional erases moves draws erase-overlapping move-overlapping
            &key check-overlapping)
    (if (propagate-output-record-changes-p
         record child mode old-position old-bounding-rectangle)
        (propagate-output-record-changes
         record child mode old-position old-bounding-rectangle
         erases moves draws erase-overlapping move-overlapping
         check-overlapping)
        (incremental-redisplay
         stream nil erases moves draws erase-overlapping move-overlapping))))

(defmethod redisplay-frame-pane
    ((frame application-frame) (pane updating-output-stream-mixin) &key force-p)
  (setf (id-counter pane) 0)
  (let ((incremental-redisplay (pane-incremental-redisplay pane)))
    (cond ((not incremental-redisplay)
           (call-next-method))
          ((or (null (updating-record pane))
               force-p)
           (setf (updating-record pane)
                 (updating-output (pane :unique-id 'top-level)
                   (call-next-method frame pane :force-p force-p))))
          ;; Implements the extension to the :incremental-redisplay
          ;; pane argument found in the Franz User Guide.
          (t (let ((record (updating-record pane)))
               (if (consp incremental-redisplay)
                   (apply #'redisplay record pane incremental-redisplay)
                   (redisplay record pane))) ))))

;;; INCREMENTAL-REDISPLAY takes as input the difference set computed by
;;; COMPUTE-DIFFERENCE-SET and updates the screen. The 5 kinds of updates are
;;; not very well defined in the spec. I understand their semantics thus:
;;;
;;; ERASES, MOVES, and DRAWS refer to records that don't overlap *with other
;;; records that survive in the current rendering*. In other words, they don't
;;; overlap with records that were not considered by COMPUTE-DIFFRENCE-SET,
;;; either because they are children of a clean updating output node or they
;;; are in another part of the output history that is not being redisplayed.
;;;
;;; Another way to think about erases, moves and draws is in terms of a
;;; possible implementation:
;;;
;;; - ERASES regions would be erased
;;; - MOVES regions would be blitted
;;; - DRAWS records would be replayed
;;;
;;; Records in ERASE-OVERLAPPING and MOVE-OVERLAPPING might overlap with any
;;; other record. They need to be implemented by erasing their region on the
;;; screen and then replaying the output history for that region. Thus, any
;;; ordering issues implied by overlapping records is handled correctly. Note
;;; that DRAWS records may be drawn without concern for the history because
;;; they additive. -- jd 2021-12-01
(defgeneric incremental-redisplay
    (stream position erases moves draws erase-overlapping move-overlapping)
  (:method ((stream updating-output-stream-mixin) position
            erases moves draws erase-overlapping move-overlapping)
    (declare (ignore position))
    (flet ((clear-bbox (bbox)
             (with-bounding-rectangle* (x1 y1 x2 y2) bbox
               (medium-clear-area stream x1 y1 x2 y2))))
      (with-output-recording-options (stream :record nil :draw t)
        (loop for (record bbox) in erases
              do (note-output-record-lost-sheet record stream)
                 (clear-bbox bbox))
        (loop for (record old-bbox) in moves
              for new-bbox = (bounding-rectangle record)
              do (clear-bbox old-bbox)
                 (replay-output-record record stream new-bbox))
        (loop for (record bbox) in draws
              do (note-output-record-got-sheet record stream)
                 (replay-output-record record stream bbox))
        (when (or erase-overlapping move-overlapping)
          (let ((history (stream-output-history stream))
                (regions +nowhere+))
            (loop for (record bbox) in erase-overlapping
                  do (note-output-record-lost-sheet record stream)
                     (setf regions (region-union regions bbox)))
            (loop for (record bbox) in move-overlapping
                  do (setf regions (region-union regions bbox)))
            (map-over-region-set-regions #'clear-bbox regions)
            (replay history stream regions)))))))
