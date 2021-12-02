(in-package #:climi)

(defmethod propagate-output-record-changes-p
    ((record cell-output-record) child mode
     old-position old-bounding-rectangle)
  t)

(defmethod propagate-output-record-changes-p
    ((record row-output-record) child mode
     old-position old-bounding-rectangle)
  t)

(defmethod propagate-output-record-changes-p
    ((record column-output-record) child mode
     old-position old-bounding-rectangle)
  t)

(defmethod propagate-output-record-changes-p
    ((record table-output-record) child mode
     old-position old-bounding-rectangle)
  t)

(defmethod propagate-output-record-changes
    ((record table-output-record) child mode
     &optional
       old-position old-bounding-rectangle
       erases moves draws erase-overlapping move-overlapping
       check-overlapping)
  (declare (ignore old-position))
  (adjust-table-cells record *standard-output*)
  (tree-recompute-extent record)
  (values nil nil nil nil nil))

(defmethod propagate-output-record-changes
    (record (child table-output-record) mode
     &optional
       old-position old-bounding-rectangle
       erases moves draws erase-overlapping move-overlapping
       check-overlapping)
  (declare (ignore old-position))
  (let ((erases (list (list child old-bounding-rectangle)))
        (moves (list (list child child)))
        (draws nil))
    (values nil nil nil erases moves)))

(defmethod note-output-record-child-changed
    ((record table-output-record) child mode
     old-position old-bounding-rectangle stream
     &optional erases moves draws erase-overlapping move-overlapping
     &key check-overlapping)
  (call-next-method))
