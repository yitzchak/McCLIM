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

#|
Incremental Redisplay Theory of Operation

Incremental redisplay compares the tree of output records before and after
calling REDISPLAY and updates those parts of the screen that are different.
UPDATING-OUTPUT forms in the code create UPDATING-OUTPUT-RECORDs in the record
tree. These records hold the before and after snapshots of the tree.  When the
display code is first run, the bodies of all the UPDATING-OUTPUT forms are
captured as closures.  Usually only the closure in the top-level output record
will ever get called, but the programmer can call REDISPLAY on any updating
output record, so we have to be prepared for that.

Redisplay proceeds thus:

All the updating output records are visited. Their state is changed to
:UPDATING and the OLD-CHILDREN slot is set to the current children.

The closure of the root updating output record is called. None of the closures
the child updating output records are called because any free variables
captured in the UPDATING-OUTPUT forms need to see the fresh bindings from this
run of the code. As UPDATING-OUTPUT forms are encountered, several things can
happen:

* The cache value of the form compares to the value stored in the record. The
record, and all the updating output records below it, are marked :clean. The
body of UPDATING-OUTPUT isn't run.

* The cache value doesn't compare. The record is marked :UPDATED, and the body
is run.

* There isn't an existing UPDATING-OUTPUT-RECORD for this UPDATING-OUTPUT
form. One is created in state :UPDATED. The body is run.

Before the top level UPDATING-OUTPUT closure returns, various output records
in the history tree might be mutated e.g., be moved. The most common case of
this is in table layout, where the records for each cell are first created and
then assigned a final location based on the dimensions of the table. But these
nodes may be children of an updating output record that has been marked
:CLEAN. Therefore, they have to be treated specially so that the rest of
incremental redisplay will consider them and not leave the screen in a trashed
state. An around method on (SETF OUTPUT-RECORD-POSITION) for display records
checks if incremental redisplay is in progress; if so, it stores the mutated
record in its closest parent UPDATING-OUTPUT record (if any). If that parent
is :CLEAN then it and any other clean parent updating output records are
marked as :UPDATED.

Next, COMPUTE-DIFFERENCE-SET compares the old and new trees. Output records
may be removed, added or moved. Their region union must be erased and then
replayed from the history to ensure the correctness. COMPUTE-DIFFERENCE-SET
compares all display output records that are the record descendants.

Finally, the old tree is walked. All updating output records in state
:UPDATING were not visited at all and thus are deleted from their parent
caches.


Problems / Future work

The complete traversals of the output history tree could be avoided by keeping
a generation number in the updating output record and updating that everytime
the node is visited.

The search for equal display nodes is expensive in part because we have no
spatially organized data structure.

|#

;;; The map from unique values to output records. Unfortunately the :ID-TEST
;;; is specified in the child updating output records, not in the record that
;;; holds the cache! So, the map lookup code jumps through some hoops to use a
;;; hash table if the child id tests allow that and if there enough records in
;;; the map to make that worthwhile.

(defclass updating-output-map-mixin ()
  ((id-map :accessor id-map :initform nil)
   (id-counter :accessor id-counter
               :documentation "The counter used to assign unique ids to
                updating output records without one.")
   (tester-function :accessor tester-function :initform 'none
                    :documentation "The function used to lookup
  updating output records in this map if unique; otherwise, :mismatch.")
   (element-count :accessor element-count :initform 0)))

;;; Complete guess...
(defparameter *updating-map-threshold* 10
  "The limit at which the id map in an updating output record switches to a
  hash table.")

;;; ((eq map-test-func :mismatch)
;;;   nil)
(defun function-matches-p (map func)
  (let ((map-test-func (tester-function map)))
    (cond ((eq map-test-func func)
           t)
          ((and (symbolp map-test-func) (symbolp func)) ; not eq
           nil)
          ((and (symbolp map-test-func) (fboundp map-test-func))
           (eq (symbol-function map-test-func) func))
          ((and (symbolp func) (fboundp func))
           (eq map-test-func (symbol-function func)))
          (t nil))))

(defun ensure-test (map test)
  (unless (function-matches-p map test)
    (explode-map-hash map)
    (setf (tester-function map) :mismatch)))

(defgeneric clear-map (map)
  (:method ((map updating-output-map-mixin))
    (setf (id-map map) nil)
    (setf (id-counter map) 0)
    (setf (element-count map) 0)))

;;; Perhaps these should be generic functions, but in the name of premature
;;; optimization they're not :)
(defun get-from-map (map value test)
  (when (eq (tester-function map) 'none)
    (return-from get-from-map nil))
  (ensure-test map test)
  (let ((map (id-map map)))
    (if (hash-table-p map)
        (gethash value map)
        (cdr (assoc value map :test test)))))


(defun maybe-convert-to-hash (map)
  (let ((test (tester-function map)))
    (when (and (not (eq test :mismatch))
               (> (element-count map) *updating-map-threshold*)
               (or (case test
                     ((eq eql equal equalp) t))
                   (eq test #'eq)
                   (eq test #'eql)
                   (eq test #'equal)
                   (eq test #'equalp)))
      (let ((new-map (make-hash-table :test test)))
        (loop
           for (key . value) in (id-map map)
           do (setf (gethash key new-map) value))
        (setf (id-map map) new-map)))))

(defun explode-map-hash (map)
  (let ((hash-map (id-map map)))
    (when (hash-table-p hash-map)
      (loop
         for key being each hash-key of hash-map using (hash-value record)
         collect (cons key record) into alist
         finally (setf (id-map map) alist)))))

(defun add-to-map (map record value test replace)
  (if (eq (tester-function map) 'none)
      (setf (tester-function map) test)
      (ensure-test map test))
  (let ((val-map (id-map map)))
    (if (hash-table-p val-map)
        (multiple-value-bind (existing-value in-table)
            (if replace
                (gethash value val-map)
                (values nil nil))
          (declare (ignore existing-value))
          (setf (gethash value val-map) record)
          (unless in-table
            (incf (element-count map))))
        (let ((val-cons (if replace
                            (assoc value val-map :test test)
                            nil)))
          (if val-cons
              (setf (cdr val-cons) record)
              (progn
                (setf (id-map map) (acons value record val-map))
                (incf (element-count map))
                (maybe-convert-to-hash map)))))))

(defun delete-from-map (map value test)
  (ensure-test map test)
  (let ((val-map (id-map map))
        (deleted nil))
    (if (hash-table-p val-map)
        (setf deleted (remhash value val-map))
        (setf (values (id-map map) deleted)
              (delete-1 value val-map :test test :key #'car)))
    (when deleted
      (decf (element-count map)))))

;;; Reset the ID counter so that updating output records without explicit IDs
;;; can be assigned one during a run of the code. I'm not sure about using
;;; reinitialize-instance for this...
(defmethod shared-initialize :after ((obj updating-output-map-mixin) slot-names
                                     &key)
  (declare (ignore slot-names))
  (setf (id-counter obj) 0))

;;; Should this have a more complete CPL, to pull in the fact that it needs a
;;; medium for graphics state?
(defclass updating-output-stream-mixin (updating-output-map-mixin
                                        extended-output-stream)
  ((redisplaying-p
    :initform nil
    :reader stream-redisplaying-p)
   (incremental-redisplay
    :initform nil
    :initarg :incremental-redisplay
    :accessor pane-incremental-redisplay)
   ;; For incremental output, holds the top level updating-output-record.
   (updating-record
    :initform nil
    :initarg :updating-record
    :accessor updating-record)))

(defmacro with-stream-redisplaying ((stream) &body body)
  `(letf (((slot-value ,stream 'redisplaying-p) t)) ,@body))

(defgeneric redisplayable-stream-p (stream)
  (:method ((stream t))
    nil)
  (:method ((stream updating-output-stream-mixin))
    t))

(defmethod pane-needs-redisplay :around ((pane updating-output-stream-mixin))
  (let ((redisplayp (call-next-method)))
    (values redisplayp (and (not (eq redisplayp :no-clear))
                            (not (pane-incremental-redisplay pane))))))

(defmethod window-clear :after ((pane updating-output-stream-mixin))
  "Get rid of any updating output records stored in the stream; they're gone
  from the screen."
  (clear-map pane))

;;; FIXME: although this inherits from COMPLETE-MEDIUM-STATE, in fact it
;;; needn't, as we only ever call SET-MEDIUM-CURSOR-POSITION on it.  Until
;;; 2006-05-28, we did also use the various medium attributes, but with the
;;; reworking of REPLAY-OUTPUT-RECORD (STANDARD-DISPLAYED-OUTPUT-RECORD) to
;;; use around methods and WITH-DRAWING-OPTIONS, they are no longer necessary.
;;;
;;; FIXME shouldn't we maintain a complete-cursor-state here? The cursor has
;;; width, height, appearance and position. -- jd 2021-11-15
(defclass updating-stream-state (complete-medium-state)
  ((cursor-x :accessor cursor-x :initarg :cursor-x :initform 0)
   (cursor-y :accessor cursor-y :initarg :cursor-y :initform 0)
   (cursor-height :accessor cursor-height :initarg :cursor-height :initform 0)))

(defmethod initialize-instance :after ((obj updating-stream-state)
                                       &key (stream nil))
  (when stream
    (setf (values (slot-value obj 'cursor-x) (slot-value obj 'cursor-y))
          (stream-cursor-position stream))
    (setf (slot-value obj 'cursor-height)
          (stream-cursor-height stream))))

(defmethod match-output-records-1 and ((state updating-stream-state)
                                       &key (cursor-x 0 x-supplied-p)
                                            (cursor-y 0 y-supplied-p)
                                            (cursor-height 0 h-supplied-p))
  (and (or (not x-supplied-p)
           (coordinate= (slot-value state 'cursor-x) cursor-x))
       (or (not y-supplied-p)
           (coordinate= (slot-value state 'cursor-y) cursor-y))
       (or (not h-supplied-p)
           (coordinate= (slot-value state 'cursor-height) cursor-height))))

(defgeneric set-medium-cursor-position (state stream)
  (:method ((state updating-stream-state) (stream updating-output-stream-mixin))
    (setf (stream-cursor-position stream)
          (values (cursor-x state) (cursor-y state)))
    (setf (stream-cursor-height stream)
          (cursor-height state))))

(defmethod medium-graphics-state ((stream updating-output-stream-mixin)
                                  &optional state)
  (if (and state (subtypep state 'updating-stream-state))
      (reinitialize-instance state :stream stream)
      (make-instance 'updating-stream-state :stream stream)))

;;; XXX Add to values we test, obviously.
;;;
;;; Well, maybe not.  The goal is to support output records that have moved
;;; but that are otherwise clean. I.e., some previous part of the output has
;;; changed (lines added or deleted, for example). If the stream cursor
;;; position is different, I'm not sure now that the code for the updating
;;; output record needs to be rerun; I think we could use only the difference
;;; in cursor position to move the record. Any other graphics state change --
;;; like a different foreground color -- should probably be handled by the
;;; programmer forcing all new output.

(defun state-matches-stream-p (record stream)
  (or (output-record-fixed-position record)
      (let ((state (start-graphics-state record))
            (cx (stream-cursor-position stream)))
        ;; Note: We don't match the y coordinate.
        (match-output-records state :cursor-x cx))))

(defclass updating-output-record-mixin (updating-output-map-mixin
                                        standard-sequence-output-record)
  ((unique-id :reader output-record-unique-id :initarg :unique-id)
   (id-test :reader output-record-id-test :initarg :id-test
            :initform #'eql)
   (cache-value :reader output-record-cache-value :initarg :cache-value)
   (cache-test :reader output-record-cache-test :initarg :cache-test
               :initform #'eql)
   (fixed-position :reader output-record-fixed-position
                   :initarg :fixed-position :initform nil)
   (displayer :accessor output-record-displayer :initarg :displayer)
   ;; Start and end cursor
   (start-graphics-state :accessor start-graphics-state
                         :initarg :start-graphics-state
                         :documentation "Graphics state needed to
   render record")
   (end-graphics-state :accessor end-graphics-state
                       :initarg :end-graphics-state
                       :documentation "Graphics state after rendering
   record; used to render non updating-output-records that follow")
   (old-children :accessor old-children
                 :documentation "Contains the output record tree for the
  current display.")
   (output-record-dirty :accessor output-record-dirty :initform :updating
          :documentation
          ":updating
           :updated
           :clean")
   (parent-cache :accessor parent-cache :initarg :parent-cache
                 :documentation "The parent cache in which this updating output
record is stored.")
   (stream :accessor updating-output-stream :initarg :stream :initform nil
           :documentation "Capture the screen in order to restrict update to
                                        visible records")
   (parent-updating-output :accessor parent-updating-output
                           :initarg :parent-updating-output :initform nil
                           :documentation "A backlink to the
updating-output-parent above this one in the tree.")
   ;; Results of (setf output-record-position) while updating
   (old-bounds :accessor old-bounds
               :initform (make-bounding-rectangle 0.0d0 0.0d0 0.0d0 0.0d0)
               :documentation "Holds the old bounds of an updating output
 record if that can no longer be determined from the old-children.")
   ;; on-screen state?
   ))

#+nyi
(defgeneric find-child-output-record (record use-old-elements record-type
                                      &rest initargs
                                      &key unique-id unique-id-test))

(defgeneric sub-record (record)
  (:method ((record updating-output-record-mixin))
    (let ((children (output-record-children record)))
      (if (zerop (length children))
          nil
          (aref children 0)))))

(defmethod output-record-start-cursor-position
    ((record updating-output-record-mixin))
  (let ((state (start-graphics-state record)))
    (values (cursor-x state) (cursor-y state))))

(defmethod* (setf output-record-start-cursor-position)
    (x y (record updating-output-record-mixin))
  (let ((state (start-graphics-state record)))
    (setf (values (cursor-x state) (cursor-y state)) (values x y))))

(defmethod output-record-end-cursor-position
    ((record updating-output-record-mixin))
  (let ((state (end-graphics-state record)))
    (values (cursor-x state) (cursor-y state))))

(defmethod* (setf output-record-end-cursor-position)
    (x y (record updating-output-record-mixin))
  (let ((state (end-graphics-state record)))
    (setf (values (cursor-x state) (cursor-y state)) (values x y))))

;;; Prevent deleted output records from coming back from the dead.
(defmethod delete-output-record :after
    ((child updating-output-record-mixin) record &optional errorp)
  (declare (ignore record errorp))
  (let ((pcache (parent-cache child)))
    (delete-from-map pcache
                     (output-record-unique-id child)
                     (output-record-id-test child))))


(defclass standard-updating-output-record (updating-output-record-mixin
                                           updating-output-record)
  ())

(defmethod print-object ((obj standard-updating-output-record) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (with-standard-rectangle* (x1 y1 x2 y2) obj
      (format stream "X ~S:~S Y ~S:~S " x1 x2 y1 y2))
    (when (slot-boundp obj 'unique-id)
      (let ((*print-length* 10)
            (*print-level* 3))
        (format stream " ~S" (output-record-unique-id obj))))))

;;; Helper function for visiting updating-output records in a tree

(defgeneric map-over-updating-output (function root use-old-records)
  (:method (function (record standard-updating-output-record) use-old-records)
    (funcall function record)
    (let ((children (cond (use-old-records
                           (when (slot-boundp record 'old-children)
                             (old-children record)))
                          (t (sub-record record)))))
      (when children
        (map-over-updating-output function children use-old-records))))
  (:method (function (record compound-output-record) use-old-records)
    (flet ((mapper (r) (map-over-updating-output function r use-old-records)))
      (declare (dynamic-extent #'mapper))
      (map-over-output-records #'mapper record)))
  (:method (function record use-old-records)
    (declare (ignore function record use-old-records))
    nil))

(defvar *current-updating-output* nil)

(defvar *no-unique-id* (cons nil nil))

(defun move-output-record (record dx dy)
  (assert (not (output-record-fixed-position record)))
  (multiple-value-bind (x y) (output-record-position record)
    (setf (output-record-position record)
          (values (+ x dx) (+ y dy))))
  ;; Cursor positions are only guaranteed to be non-nil for text
  ;; output records (16.2.1 The Basic Output Record Protocol)
  (multiple-value-bind (x y) (output-record-start-cursor-position record)
    (when (and x y)
      (setf (output-record-start-cursor-position record)
            (values (+ x dx) (+ y dy)))))
  (multiple-value-bind (x y) (output-record-end-cursor-position record)
    (when (and x y)
      (setf (output-record-end-cursor-position record)
            (values (+ x dx) (+ y dy))))))

(defmethod invoke-updating-output ((stream updating-output-stream-mixin)
                                   continuation
                                   record-type
                                   unique-id id-test cache-value cache-test
                                   &key (fixed-position nil) (all-new nil)
                                        (parent-cache nil))
  (force-output stream)
  (setf parent-cache (or parent-cache *current-updating-output* stream))
  (when (eq unique-id *no-unique-id*)
    (setq unique-id (incf (id-counter parent-cache))))
  (let ((record (get-from-map parent-cache unique-id id-test)))
    (cond ((or all-new (null record))
           ;; This case covers the outermost updating-output too.
           (with-new-output-record
               (stream record-type *current-updating-output*
                       :unique-id unique-id
                       :id-test id-test
                       :cache-value cache-value
                       :cache-test cache-test
                       :fixed-position fixed-position
                       :displayer continuation
                       :parent-cache parent-cache
                       :stream stream
                       :parent-updating-output *current-updating-output*)
             (setq record *current-updating-output*)
             (setf (start-graphics-state record) (medium-graphics-state stream))
             (%invoke-updating record stream continuation)
             (setf (end-graphics-state record) (medium-graphics-state stream))
             (add-to-map parent-cache record  unique-id id-test all-new)))
          ((or (not (state-matches-stream-p record stream))
               (not (funcall cache-test cache-value (output-record-cache-value record))))
           (let ((*current-updating-output* record))
             (setf (start-graphics-state record) (medium-graphics-state stream))
             (compute-new-output-records-1 record stream continuation)
             (setf (slot-value record 'cache-value) cache-value)
             (setf (end-graphics-state record) (medium-graphics-state stream))
             (setf (parent-cache record) parent-cache)
             (setf (output-record-displayer record) continuation)))
          ;; It doesn't need to be updated, but it does go into the parent's
          ;; sequence of records.
          ((output-record-fixed-position record)
           (setf (output-record-parent record) nil)
           (map-over-updating-output (lambda (r)
                                       (setf (output-record-dirty r) :clean))
                                     record
                                     nil)
           (add-output-record record (stream-current-output-record stream))
           (setf (parent-cache record) parent-cache)
           (setf (output-record-displayer record) continuation))
          ;; It doesn't need to be updated, but it does go into the parent's
          ;; sequence of records. The record also needs to be moved.
          (t
           (multiple-value-bind (cx cy)
               (stream-cursor-position stream)
             (multiple-value-bind (sx sy)
                 (output-record-start-cursor-position record)
               (let ((dx (- cx sx))
                     (dy (- cy sy)))
                 (unless (zerop dy)
                   (move-output-record record dx dy))
                 (let ((tag (if (= dx dy 0) :clean :moved)))
                   (map-over-updating-output
                    (lambda (r)
                      (unless (eq r record)
                        (incf (slot-value (start-graphics-state r) 'cursor-x) dx)
                        (incf (slot-value (start-graphics-state r) 'cursor-y) dy)
                        (incf (slot-value (end-graphics-state r) 'cursor-x) dx)
                        (incf (slot-value (end-graphics-state r) 'cursor-y) dy))
                      (setf (output-record-dirty r) tag))
                    record
                    nil)
                   (setf (output-record-parent record) nil)
                   (add-output-record record (stream-current-output-record stream))
                   (set-medium-cursor-position (end-graphics-state record) stream)
                   (setf (parent-cache record) parent-cache)
                   (setf (output-record-displayer record) continuation)))))))
    record))

;;; The Franz user guide says that updating-output does &allow-other-keys, and
;;; some code I've encountered does mention other magical arguments, so we'll
;;; do the same. -- moore
(defun force-update-cache-test (a b)
  (declare (ignore a b))
  nil)

(defmacro updating-output
    ((stream
      &key (unique-id '*no-unique-id*) (id-test '#'eql)
      (cache-value ''no-cache-value cache-value-supplied-p)
      (cache-test '#'eql)
      (fixed-position nil fixed-position-p)
      (all-new nil all-new-p)
      (parent-cache nil parent-cache-p)
      (record-type ''standard-updating-output-record)
      &allow-other-keys)
     &body body)
  (when (eq stream t)
    (setq stream '*standard-output*))
  (unless cache-value-supplied-p
    (setq cache-test '#'force-update-cache-test))
  (let ((func (gensym "UPDATING-OUTPUT-CONTINUATION")))
    `(flet ((,func (,stream)
              (declare (ignorable ,stream))
              ,@body))
       (invoke-updating-output ,stream (function ,func) ,record-type ,unique-id
                               ,id-test ,cache-value ,cache-test
                               ,@ (and fixed-position-p
                                       `(:fixed-position ,fixed-position))
                               ,@(and all-new-p `(:all-new ,all-new))
                               ,@(and parent-cache-p
                                      `(:parent-cache ,parent-cache))))))

;;; Support for explicitly changing output records

(defun mark-updating-output-changed (record)
  (when (and (not (eq record *current-updating-output*))
             (eq (output-record-dirty record) :clean))
    (setf (output-record-dirty record) :updated)
    (let ((parent (parent-updating-output record)))
      (assert (not (null parent)) () "parent of ~S null." record)
      (mark-updating-output-changed parent))))

(defgeneric propagate-to-updating-output
    (record child mode old-bounding-rectangle)
  (:method ((record updating-output-record-mixin) child mode old-bbox)
    (when (and (eq mode :move)
               (eq (output-record-dirty record) :clean))
      (mark-updating-output-changed record)))
  (:method ((record output-record) child mode old-bbox)
    (when-let ((parent (output-record-parent record)))
      (propagate-to-updating-output parent child mode old-bbox))))

(defmethod* (setf output-record-position) :around
  (nx ny (record displayed-output-record))
  (with-bounding-rectangle* (x1 y1 x2 y2) record
    (multiple-value-prog1 (call-next-method)
      (unless (and (coordinate= x1 nx) (coordinate= y1 ny))
        (when-let* ((stream (and (slot-exists-p record 'stream)
                                 (slot-value  record 'stream)))
                    (parent (output-record-parent record)))
          (when (stream-redisplaying-p stream)
            (propagate-to-updating-output
             parent record :move (make-bounding-rectangle x1 y1 x2 y2))))))))
