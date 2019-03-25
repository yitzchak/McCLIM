(in-package #:climi)

(defclass standard-port (clim:basic-port)
  ((mirrored-sheet->current-pointer-cursor :initform (make-hash-table :test #'eq))
   (selections :initform (make-hash-table) :reader standard-port-selections)))

(defmethod port-lookup-current-pointer-cursor ((port standard-port) sheet)
  (gethash sheet (slot-value port 'mirrored-sheet->current-pointer-cursor)))

(defmethod set-sheet-pointer-cursor :before ((port standard-port) sheet cursor)
  (setf (gethash sheet (slot-value port 'mirrored-sheet->current-pointer-cursor)) cursor))

(defun stored-object (port selection)
  (check-type port standard-port)
  (check-type selection symbol)
  (gethash selection (standard-port-selections port)))

(defsetf stored-object (port selection) (value)
  (alexandria:once-only (port selection)
    `(progn
       (check-type ,port standard-port)
       (check-type ,selection symbol)
       (setf (gethash ,selection (standard-port-selections ,port)) ,value))))

(defun remove-stored-object (port selection)
  (check-type port standard-port)
  (check-type selection symbol)
  (remhash selection (standard-port-selections port)))

;;;
;;; Backend developers:
;;;
;;;   Backend developers should write methods for CLIMB:CLEAR-SELECTION,
;;;   CLIMB:PUBLISH-SELECTION and CLIMB:REQUEST-SELECTION for each box which
;;;   should have a special behavior associated with it.
;;;
;;;   CLIMB:SELECTION-OBJECT is a class with the following accessors:
;;;
;;;     CLIMB:SELECTION-OBJECT-CONTENT - content to publish/clear or a selection for rquest
;;;     CLIMB:SELECTION-OBJECT-TYPES - presentation types associated with the content
;;;     CLIMB:SELECTION-OBJECT-OWNER - sheet associated with the content
;;;
;;;   CLIMB:CLEAR-SELECTION publisher selection
;;;
;;;     PUBLISHER should be specialized on developed port and SELECTION should
;;;     have EQL-specialization on a symbol denoting the box. Result of this
;;;     operation should be making the selection unavailable for new requests.
;;;
;;;   CLIMB:PUBLISH-SELECTION publisher selection object &optional type
;;;
;;;     PUBLISHER should be specialized on developed port, SELECTION should have
;;;     EQL-specialization on a symbol denoting the box and OBJECT should be
;;;     specialized on CLIMB:SELECTION-OBJECT. Optional arugment TYPE is
;;;     ignored. Function is responsible for making the content available in a box
;;;     SELECTION for new requests.
;;;
;;;   CLIMB:REQUEST-SELECTION requester selection acceptable-types
;;;
;;;     REQUESTER should be specialized on developed port. SELECTION should have
;;;     EQL-specialization on a symbol denoting the box. ACCEPTABLE-TYPES is
;;;     always a list of presentation types which are to be accepted. When content
;;;     for the request is available CLIME:SELECTION-REQUEST-RESPONSE-EVENT should
;;;     be queued in a port. If there is none then no action is taken.

(defmethod clear-selection ((port standard-port) selection object)
  (declare (ignore object))
  (remove-stored-object port selection))

(defmethod publish-selection ((port standard-port) selection (object selection-object)
                              &optional pt)
  (declare (ignore pt))
  (setf (stored-object port selection) object))

(defmethod request-selection ((port standard-port) selection request)
  (alexandria:when-let* ((object (stored-object port selection)))
    (dolist (acceptable-type (selection-object-types request))
      (dolist (candidate-type (selection-object-types object))
        (when (presentation-subtypep candidate-type acceptable-type)
          (return-from request-selection
            (values (selection-object-content object) candidate-type t)))))))
