(in-package :clim-internals)

(define-presentation-generic-function %convert-selection-content convert-selection-content
  (type-key parameters options object type output-type check-only))

(define-presentation-method convert-selection-content (obj (type t) output-type check-only)
  nil)

(define-presentation-method convert-selection-content (obj (type string) (output-type (eql :string)) check-only)
  (check-type obj string)
  obj)

(defun convert-selection-content (obj output-type &key type check-only)
  (funcall-presentation-generic-function convert-selection-content
                                         obj
                                         (or type (presentation-type-of obj))
                                         output-type
                                         check-only))
