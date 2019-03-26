(in-package :clim-clx)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CLX implementation of clipboard management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Port mixin

(defclass clx-clipboard-port-mixin ()
  (;; For requests which await objects from other applications:
   (outstanding-request :initform nil :accessor clipboard-outstanding-request)))

(macrolet ((thunk (selection)
             `(defmethod climb:publish-selection ((port clx-clipboard-port-mixin)
                                                  (selection (eql ,selection))
                                                  (object climb:selection-object)
                                                  &optional presentation-type)
                (declare (ignore port presentation-type))
                (let ((window (sheet-direct-xmirror (climb:selection-object-owner object))))
                  ;; We're not actually supposed to call set-selection-owner without
                  ;; a timestamp due to the following statemnt in ICCCM:
                  ;;
                  ;;     Clients attempting to acquire a selection must set the time
                  ;;     value of the SetSelectionOwner request to the timestamp of
                  ;;     the event triggering the acquisition attempt, not to
                  ;;     CurrentTime. A zero-length append to a property is a way to
                  ;;     obtain a timestamp for this purpose; the timestamp is in
                  ;;     the corresponding PropertyNotify event.
                  ;;
                  ;; The reasons for his seems to be to ensure that the ownership is
                  ;; actually transferred correctly. This shouldn't be a major issue
                  ;; in practice, and it significantly simplifies the
                  ;; implementation.
                  (xlib:set-selection-owner (xlib:window-display window) selection window nil)
                  (if (eq (xlib:selection-owner (xlib:window-display window) selection) window)
                      (call-next-method)
                      (warn "Couldn't set X11 selection ~s owner to ~s." selection window))))))
  (thunk :primary)
  (thunk :clipboard))

(macrolet ((thunk (selection)
             `(defmethod climb:clear-selection ((port clx-clipboard-port-mixin)
                                                (selection (eql ,selection))
                                                (object selection-object))
                (let ((window (sheet-direct-xmirror (climb:selection-object-owner object))))
                  (xlib:set-selection-owner (xlib:window-display window) selection nil nil)
                  (call-next-method)))))
  (thunk :primary)
  (thunk :clipboard))

;;;
;;;  Paste support
;;;
;;;  When a client requests the content of the clipboard or selection,
;;;  a TYPE parameter is used to indicate what type the caller expects
;;;  to receive. This value is a keyword (or list of keywords) that
;;;  includes the values :STRING, :HTML and :IMAGE.
;;;
;;;  When the function REQUEST-SELECTION-CONTENT or
;;;  REQUEST-CLIPBOARD-CONTENT arrives, the requestor information is
;;;  stored in the port, and a TARGETS request is then sent to the
;;;  selection owner. As per the ICCCM, when a client receives a
;;;  TARGETS request, it should return a list of supported types.
;;;
;;;  When the TARGETS response arrives, the list of available types is
;;;  compared with the list of requested types. If there is no match,
;;;  no further processis occurs. If any types match, the first match
;;;  is chosen and another request is sent to the selection owner with
;;;  the type set to the matched type. Once this response arrives, it
;;;  is delivered as an event to the pane that performed the request.

(macrolet ((thunk (selection)
             `(defmethod request-selection ((port clx-clipboard-port-mixin)
                                            (selection (eql ,selection))
                                            (requested-object selection-object))
                (setf (clipboard-outstanding-request port) requested-object)
                (let ((window (sheet-direct-xmirror (selection-object-owner requested-object))))
                  (xlib:convert-selection selection :targets window :mcclim nil)))))
  (thunk :primary)
  (thunk :clipboard))

(defun representation-type-to-native (type)
  (case type
    (:string '(:utf8_string :text :string :|text/plain;charset=utf-8| :|text/plain|))
    (:html '(:|text/html|))
    (:image '(:|image/png| :|image/jpeg|))))

(defun process-selection-request (port window target property requestor selection time)
  (alexandria:when-let ((stored-object (climi::stored-object port selection)))
    (when (eq (sheet-direct-xmirror (selection-object-owner stored-object)) window)
      (let ((content (selection-object-content stored-object))
            (presentation-type (car (climb:selection-object-types stored-object))))
        (flet ((send-reply-event (&optional omit-prop)
                 (xlib:send-event requestor :selection-notify nil
                                  :window requestor
                                  :event-window requestor
                                  :selection selection
                                  :target target
                                  :property (if omit-prop nil property)
                                  :time time))
               (make-targets-response ()
                 (let ((display (clx-port-display port))
                       (types (loop
                                 for output-type in '(:string :html)
                                 when (clime:convert-selection-content content output-type
                                                                       :type presentation-type
                                                                       :check-only t)
                                 append (representation-type-to-native output-type))))
                   (mapcar (lambda (v)
                             (xlib:intern-atom display v))
                           (remove-duplicates (cons :targets types))))))
          (case target
            ((:targets)
             (let ((targets (make-targets-response)))
               (xlib:change-property requestor property targets :atom 32)
               (send-reply-event)))
            ((:utf8_string :text :string :|text/plain;charset=utf-8| :|text/plain|)
             (let ((content-as-string (clime:convert-selection-content content :string :type presentation-type)))
               (xlib:change-property requestor property (babel:string-to-octets content-as-string :encoding :utf-8) :string 8)
               (send-reply-event)))
            ((:|text/html|)
             (let ((s (format nil "<meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\">~a"
                              (clime:convert-selection-content content :html :type presentation-type))))
               (xlib:change-property requestor property
                                     (babel:string-to-octets s :encoding :utf-8)
                                     :string 8))
             (send-reply-event))
            (t
             (send-reply-event t)))
          nil)))))

(defun process-selection-notify (port selection target property time)
  (alexandria:when-let ((request (clipboard-outstanding-request port)))
    (unless (eq (climb:selection-object-content request) selection)
      (return-from process-selection-notify))
    (let ((window (sheet-direct-xmirror (climb:selection-object-owner request))))
      (flet ((convert-image-array (array type)
               (flexi-streams:with-input-from-sequence (s array)
                 (let ((image-content (ecase type
                                        (:png (opticl:read-png-stream s))
                                        (:jpeg (opticl:read-jpeg-stream s)))))
                   (make-instance 'clime:image-pattern
                                  :array (climi::convert-opticl-img image-content)))))
             (process-targets-reply ()
               (alexandria:when-let ((request (clipboard-outstanding-request port)))
                 (when (eq (climb:selection-object-content request) selection)
                   (let* ((display (xlib:window-display window))
                          (targets (mapcar (lambda (v)
                                             (xlib:atom-name display v))
                                           (xlib:get-property window :mcclim)))
                          (selected-type (loop
                                            named outer
                                            for request-type in (climb:selection-object-types request)
                                            for v = (loop
                                                       for type in (representation-type-to-native request-type)
                                                       when (member type targets)
                                                       return type)
                                            when v
                                            return v)))
                     (if selected-type
                         (xlib:convert-selection selection selected-type window :mcclim time)
                         ;; ELSE: The clipboard doesn't support content of this type,
                         ;; send a negative response here.
                         nil)))))
             (process-clipboard-reply (content type)
               (alexandria:when-let ((request (clipboard-outstanding-request port)))
                 (let* ((sheet (climb:selection-object-owner request))
                        (event (make-instance 'clime:selection-request-response-event
                                              :content content :type type :sheet sheet)))
                   (setf (clipboard-outstanding-request port) nil)
                   (queue-event sheet event)))))
        (case target
          ((:targets)
           (process-targets-reply))
          ((:utf8_string :text :string  :|text/plain;charset=utf-8| :|text/plain|)
           (let ((content (babel:octets-to-string (coerce (xlib:get-property window property) '(vector (unsigned-byte 8)))
                                                  :encoding :utf-8)))
             (process-clipboard-reply content :string)))
          ((:|text/html|)
           (let ((content (babel:octets-to-string (coerce (xlib:get-property window property) '(vector (unsigned-byte 8)))
                                                  :encoding :utf-8)))
             (process-clipboard-reply content :html)))
          ((:|image/png|)
           (let ((image (convert-image-array (coerce (xlib:get-property window property) '(vector (unsigned-byte 8))) :png)))
             (process-clipboard-reply image :image)))
          ((:|image/jpeg|)
           (let ((image (convert-image-array (coerce (xlib:get-property window property) '(vector (unsigned-byte 8))) :jpeg)))
             (process-clipboard-reply image :image)))
          (t
           nil))))))

(defun process-selection-clear (port selection)
  (climb:clear-selection port selection nil))
