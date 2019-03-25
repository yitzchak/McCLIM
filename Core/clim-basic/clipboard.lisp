(in-package #:climi)

;;; How selection boxes work
;;;
;;; User may use any amount of selection boxes designated by a symbol. Some boxes
;;; may have a special treatment – for instance on x11 :clipboard and :primary are
;;; integrated with the display server boxes. Backends are allowed to
;;; EQL-specialize other boxes too. If a box is not specialized then it is
;;; available only for frames which are in the same lisp image.
;;;
;;; Some boxes may have some default treatment:
;;;
;;;   :LOCAL-SELECTION
;;;
;;;     Active selection on clim-stream made with a gesture.
;;;
;;;   :PRIMARY
;;;
;;;     Current selection on a display server.  Gesture on clim-stream will change
;;;     the selection, but another window may overwrite it. Content of this box is
;;;     pasted when appropriate gesture is made.
;;;
;;;   :CLIPBOARD
;;;
;;;     Clipboard holds a result of the last kill operation which may be used in a
;;;     yank operation. See CLIME:CONVERT-CLIPBOARD-CONTENT for interaction with
;;;     other applications.
;;;
;;; End users:
;;;
;;;   Some predefined gestures are definedto make the end user life easier. For
;;;   text-selection:
;;;
;;;     Shift-Mouse-L: select text by dragging cursor (sets the box :primary)
;;;     Shift-Mouse-R: move nearest point of existing selection
;;;     Shift-Mouse-M: paste content of the box :primary
;;;
;;;   On text-editing fields kill command puts content in the box :clipboard and
;;;   yank command takes the content from said box and puts it in the buffer.

;;; Instances of class SELECTION-OBJECT are used for both requesting and
;;; publishing the selection. Methods specialized on port are specialized on this
;;; class of object and it is a responsibility of methods specialized on sheets to
;;; allocate these objects for functions CLEAR-SELECTION, PUBLISH-SELECTION and
;;; REQUEST-SELECTION.
;;;
;;; SELECTION-OBJECT-CONTENT - content to publish/clear or a selection for rquest
;;; SELECTION-OBJECT-TYPES - presentation types associated with the content
;;; SELECTION-OBJECT-OWNER - sheet associated with the content
(defclass selection-object ()
  ((content :initarg :content :reader selection-object-content)
   (types   :initarg :types   :reader selection-object-types)
   (owner   :initarg :owner   :reader selection-object-owner)))

;;; Clears SELECTION (only when PUBLISHER is its owner). When called on port
;;; selection is cleared unconditionally.
(defgeneric clear-selection (publisher selection object)
  (:documentation "Wipe the selection box.")
  (:method ((sheet basic-sheet) selection object)
    (declare (ignore object))
    (alexandria:when-let*
        ((port (port sheet))
         (object (stored-object port selection)))
      (when (eql (selection-object-owner object) sheet)
        (clear-selection port selection object)))))

;;; Stores OBJECT in a box SELECTION. PRESENTATION-TYPE defaults to
;;; (PRESENTATION-TYPE-OF OBJECT). Object may be published under many presentation
;;; types in which case argument should hold a list with them.
(defgeneric publish-selection (publisher selection object &optional presentation-types)
  (:documentation "Publish the object in the selection box.")
  (:method ((sheet basic-sheet) selection object
            &optional (presentation-types (list (presentation-type-of object))))
    (publish-selection (port sheet)
                       selection
                       (make-instance 'selection-object
                                      :content object
                                      :types (alexandria:ensure-list presentation-types)
                                      :owner sheet))))

;; Function returns immedietely. If SELECTION matching one of ACCEPTABLE-TYPES is
;; available CLIME:SELECTION-REQUEST-RESULT-EVENT is queued for REQUESTER sheet.
(defgeneric request-selection (requester selection request)
  (:documentation "Request an object from the selection box.")
  (:method ((sheet basic-sheet) selection acceptable-types)
    (multiple-value-bind (content type present-p)
        (request-selection (port sheet)
                           selection
                           (make-instance 'selection-object
                                          :content selection
                                          :types (alexandria:ensure-list acceptable-types)
                                          :owner sheet))
      (when present-p
        (queue-event sheet (make-instance 'clime:selection-request-response-event
                                          :type type :content content :sheet sheet)))
      (values content type present-p))))
