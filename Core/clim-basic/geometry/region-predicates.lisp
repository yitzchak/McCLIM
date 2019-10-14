(in-package #:climi)

;;; This file contains implementation of the region predicate protocol
;;; for several region classes. A few rules to follow:
;;;
;;; - use coordinate functions for comparisons
;;; - when possible provide a method for a protocol class, and when
;;;   beneficial provide (also) a method for a standard class
;;; - all "region" arguments must be specialized
;;; - the most general specialization is bounding-rectangle
;;;
;;; Regions which must be handled:
;;;
;;;   - 0 dimensions: point
;;;   - 1 dimensions: polyline, elliptical-arc
;;;   - 2 dimensions: polygon, ellipse
;;;
;;; Additionally a line and a rectangle should be handled (they are
;;; frequently used special cases of a polyline and a polygon).
;;;
;;; Methods should be defined from the most general to the most
;;; specific. Methods specialized on REGION, methods being a
;;; consequence of the dimensionality rule and finally methods
;;; specific to a particular region type. Unbounded regions and region
;;; sets have their methods defined elsewhere.


(defmethod region-contains-position-p ((self point) x y)
  (and (coordinate= (point-x self) x)
       (coordinate= (point-y self) y)))

(defmethod region-contains-position-p ((self polyline) x y)
  (setf x (coordinate x)
        y (coordinate y))
  (block nil
    (map-over-polygon-segments
     (lambda (x1 y1 x2 y2)
       (when (segment-contains-point-p x1 y1 x2 y2 x y)
         (return t)))
     self)
    nil))

(defmethod region-contains-position-p ((self line) x y)
  (multiple-value-bind (x1 y1) (line-start-point* self)
    (multiple-value-bind (x2 y2) (line-end-point* self)
      (segment-contains-point-p x1 y1 x2 y2 x y))))

(defmethod region-contains-position-p ((self elliptical-arc) x y)
  (nest
   (multiple-value-bind (cx cy) (ellipse-center-point* self))
   (let ((alpha (ellipse-start-angle self))
         (omega (ellipse-end-angle self))
         (polar->screen (polar->screen self)))
     (and (angle-contains-point-p alpha omega (- x cx) (- y cy))
          (nest (multiple-value-bind (polar-x polar-y)
                    (untransform-position polar->screen x y))
                ;; FIXME we don't need to factor the additonal epsilon
                ;; but rotated elliptoids are naively rendered in clx.
                (multiple-value-bind (polar-dx polar-dy)
                    (untransform-distance polar->screen 1 1))
                (coordinate-between*
                 (- 1 (+ (square polar-dx) (square polar-dy)))
                 (+ (square polar-x) (square polar-y))
                 (+ 1 (+ (square polar-dx) (square polar-dy)))))))))

(defmethod region-contains-position-p ((region polygon) x y)
  (and (region-contains-position-p (bounding-rectangle region) x y)
       ;; The following algorithm is a Winding Number (wn) method implementation
       ;; based on a description by Dan Sunday "Inclusion of a Point in a
       ;; Polygon" (http://geomalgorithms.com/a03-_inclusion.html).
       (flet ((is-left (x0 y0 x1 y1 x2 y2)
                (- (* (- x1 x0) (- y2 y0))
                   (* (- x2 x0) (- y1 y0)))))
         (let ((x (coordinate x))
               (y (coordinate y))
               (wn 0))
           (map-over-polygon-segments
            (lambda (x1 y1 x2 y2)
              ;; Algorithm is not predictible for polygon edges - we
              ;; need to test for them explicitly. -- jd 2019-09-27
              (when (segment-contains-point-p x1 y1 x2 y2 x y)
                (return-from region-contains-position-p t))
              (if (<= y1 y)
                  (when (and (> y2 y)
                             (> (is-left x1 y1 x2 y2 x y) 0))
                    (incf wn))
                  (when (and (<= y2 y)
                             (< (is-left x1 y1 x2 y2 x y) 0))
                    (decf wn))))
            region)
           (not (zerop wn))))))

(defmethod region-contains-position-p ((self rectangle) x y)
  (multiple-value-bind (x1 y1 x2 y2)
      (rectangle-edges* self)
    (and (coordinate-between* x1 x x2)
         (coordinate-between* y1 y y2))))

(defmethod region-contains-position-p ((self ellipse) x y)
  (nest
   (multiple-value-bind (cx cy) (ellipse-center-point* self))
   (let ((alpha (or (ellipse-start-angle self) 0))
         (omega (or (ellipse-end-angle self) (* 2 pi)))
         (polar->screen (polar->screen self)))
     (and (angle-contains-point-p alpha omega (- x cx) (- y cy))
          (nest (multiple-value-bind (polar-x polar-y)
                    (untransform-position polar->screen x y))
                ;; FIXME we don't need to factor the additonal epsilon
                ;; but rotated elliptoids are naively rendered in clx.
                (multiple-value-bind (polar-dx polar-dy)
                    (untransform-distance polar->screen 1 1))
                (coordinate<= (+ (square polar-x) (square polar-y))
                              (+ 1 (square polar-dx) (square polar-dy))))))))


;;   REGION-INTERSECTS-REGION-P region1 region2
;;
;;        Returns nil if region-intersection of the two regions region1 and
;;        region2 would be +nowhere+; otherwise, it returns t.
;;
;;        aka region1 and region2 are not disjoint aka AB /= 0
;;

;;; general rules

;;; fallback method
(defmethod region-intersects-region-p ((a bounding-rectangle) (b bounding-rectangle))
  (not (region-equal +nowhere+ (region-intersection a b))))

(defmethod region-intersects-region-p :around ((a bounding-rectangle) (b bounding-rectangle))
  (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* a)
    (multiple-value-bind (u1 v1 u2 v2) (bounding-rectangle* b)
      (and (<= u1 x2) (<= x1 u2)
           (<= v1 y2) (<= y1 v2)
           (call-next-method)))))

(defmethod region-intersects-region-p ((a path) (b area))
  (region-intersects-region-p b a))

;;; points
(defmethod region-intersects-region-p ((a point) (b bounding-rectangle))
  (multiple-value-bind (x y) (point-position a)
    (region-contains-position-p a x y)))

(defmethod region-intersects-region-p ((a bounding-rectangle) (b point))
  (region-intersects-region-p b a))

;;; paths vs paths
(defmethod region-intersects-region-p ((a polyline) (b polyline))
  ;; If an intersection is a point it doesn't count as such.
  (map-over-polygon-segments
   (lambda (ax ay bx by)
     (map-over-polygon-segments
      (lambda (cx cy dx dy)
        (unless (eql t (segment-difference ax ay bx by cx cy dx dy))
          (return-from region-intersects-region-p t)))
      b))
   a)
  nil)

(defmethod region-intersects-region-p ((a polyline) (b elliptical-arc))
  (declare (ignore a b))
  nil)

(defmethod region-intersects-region-p ((a elliptical-arc) (b polyline))
  (declare (ignore a b))
  nil)

(defmethod region-intersects-region-p ((a elliptical-arc) (b elliptical-arc))
  (multiple-value-bind (cx1 cy1 rh1 rv1 phi1)
      (ellipse-simplified-representation a)
    (multiple-value-bind (cx2 cy2 rh2 rv2 phi2)
        (ellipse-simplified-representation b)
      (and (coordinate= cx1 cx2)      ; centers match
           (coordinate= cy1 cy2)
           (coordinate= rh1 rh2)      ; radii match
           (coordinate= rv1 rv2)
           (= phi1 phi2)              ; rotation match
           (let ((a-alpha (or (ellipse-start-angle a) 0))
                 (b-alpha (or (ellipse-start-angle b) 0))
                 (a-omega (or (ellipse-end-angle a) (* 2 pi)))
                 (b-omega (or (ellipse-end-angle b) (* 2 pi))))
             (or (= a-alpha b-alpha)
                 (and (< b-alpha a-alpha)
                      (or (< b-omega b-alpha)
                          (> b-omega a-alpha)))
                 (and (< a-alpha b-alpha)
                      (or (< a-omega a-alpha)
                          (> a-omega b-alpha)))))))))

;;; areas vs paths
(defmethod region-intersects-region-p ((a polygon) (b polyline))
  (let ((points (polygon-points a))
        last-overcut)
    (map-over-polygon-segments
     (lambda (x1 y1 x2 y2)
       (setf last-overcut nil)
       (map-over-overcuts-line/polygon
        (lambda (overcut)
          (clampf overcut 0d0 1d0)
          (when (null last-overcut)
            (setf last-overcut overcut))
          (when (/= last-overcut overcut)
            (return-from region-intersects-region-p t)))
        x1 y1 x2 y2 points))
     b))
  nil)

(defmethod region-intersects-region-p ((a ellipse) (b polyline))
  (let ((alpha (ellipse-start-angle a))
        (omega (ellipse-end-angle a)))
    (map-over-polygon-segments
     (lambda (bx1 by1 bx2 by2)
       (multiple-value-bind (ax1 ay1 ax2 ay2)
           (cond ((= bx1 bx2) (intersection-vline/ellipse a bx1))
                 ((= by1 by2) (intersection-hline/ellipse a by1))
                 (t (intersection-line/ellipse a bx1 by1 bx2 by2)))
         (when (and (every #'realp (list ax1 ay1 ax2 ay2))
                    (segment-contains-segment-p bx1 by1 bx2 by2
                                                ax1 by1 bx2 by2))
           ;; Following block checks if the intersection segment at
           ;; least partially belongs to the slice denoted by start
           ;; and end angles.
           (multiple-value-bind (cx cy) (ellipse-center-point* a)
             (when (or (and (coordinate/= ax1 cx)
                            (coordinate/= ay1 cy)
                            (angle-contains-point-p alpha omega (- ax1 cx) (- ay1 cy)))
                       (and (coordinate/= ax2 cx)
                            (coordinate/= ay2 cy)
                            (angle-contains-point-p alpha omega (- ax2 cx) (- ay2 cy))))
               (return-from region-intersects-region-p t))))))
     b))
  nil)

;;; FIXME the following cases are currently handled by a fallback method.

;; (defmethod region-intersects-region-p ((a rectangle) (b elliptical-arc)))
;; (defmethod region-intersects-region-p ((a polygon) (b elliptical-arc)))
;; (defmethod region-intersects-region-p ((a ellipse) (b elliptical-arc)))

;;; areas vs areas
(defmethod region-intersects-region-p ((a rectangle) (b rectangle))
  (declare (ignorable a b))
  ;; For rectangles an :around method specialized on bounding
  ;; rectangles test is sufficient, so if we wind up here, we may
  ;; simply return T.
  #+ (or)
  (multiple-value-bind (x1 y1 x2 y2) (rectangle-edges* a)
    (multiple-value-bind (u1 v1 u2 v2) (rectangle-edges* b)
      (and (<= u1 x2) (<= x1 u2)
           (<= v1 y2) (<= y1 v2))))
  t)

;;; FIXME the following cases are currently handled by a fallback method.

;; (defmethod region-intersects-region-p ((a polygon) (b polygon)))
;; (defmethod region-intersects-region-p ((a polygon) (b ellipse)))
;; (defmethod region-intersects-region-p ((a ellipse) (b polygon)))
;; (defmethod region-intersects-region-p ((a ellipse) (b ellipse)))


;;   REGION-CONTAINS-REGION-P region1 region2
;;
;;        Returns T if all points in the region are members of the
;;        region B; otherwise, it returns nil.
;;
;;        aka region2 ist teilmenge von region1  aka B\A = 0
;;

;;; "generic" version
(defmethod region-contains-region-p ((a bounding-rectangle) (b bounding-rectangle))
  (or (eq a b)
      (region-equal +nowhere+ (region-difference b a))))

(defmethod region-contains-region-p ((a standard-ellipse) (b standard-ellipse))
  (multiple-value-bind (bcx bcy) (ellipse-center-point* b)
    (and (region-contains-position-p a bcx bcy)
         (null (intersection-ellipse/ellipse a b))
         (or (null (ellipse-start-angle a))
             (multiple-value-bind (sx sy) (%ellipse-angle->position a (ellipse-start-angle a))
               (multiple-value-bind (ex ey) (%ellipse-angle->position a (ellipse-end-angle a))
                 (multiple-value-bind (cx cy) (ellipse-center-point* a)
                   (and (null (region-intersection b (make-line* sx sy cx cy)))
                        (null (region-intersection b (make-line* ex ey cx cy)))))))))))

;;; Ellipse is a convex object. That Implies that if each of the rectangle
;;; vertexes lies inside it, then whole rectangle fits as well. We take a
;;; special care for ellipses with start/end angle.
(defmethod region-contains-region-p ((a standard-ellipse) (b standard-rectangle))
  (with-standard-rectangle (x1 y1 x2 y2) b
    (if (null (ellipse-start-angle a))
        (and (region-contains-position-p a x1 y1)
             (region-contains-position-p a x2 y1)
             (region-contains-position-p a x1 y2)
             (region-contains-position-p a x2 y2))
        (flet ((fits (l) (region-equal l (region-intersection l a))))
          (and (fits (make-line* x1 y1 x2 y1))
               (fits (make-line* x2 y1 x2 y2))
               (fits (make-line* x2 y2 x1 y2))
               (fits (make-line* x1 y2 x1 y1)))))))

(defmethod region-contains-region-p ((a standard-ellipse) (polygon standard-polygon))
  (if (null (ellipse-start-angle a))
      (map-over-polygon-coordinates
       #'(lambda (x y)
           (unless (region-contains-position-p a x y)
             (return-from region-contains-region-p nil)))
       polygon)
      (map-over-polygon-segments
       #'(lambda (x1 y1 x2 y2
                  &aux (line (make-line* x1 y1 x2 y2)))
           (unless (region-equal line (region-intersection line a))
             (return-from region-contains-region-p nil)))
       polygon))
  T)

(defmethod region-contains-region-p ((a bounding-rectangle) (b point))
  (region-contains-position-p a (point-x b) (point-y b)))



;; "generic" version
(defmethod region-equal ((a bounding-rectangle) (b bounding-rectangle))
  (region-equal +nowhere+ (region-exclusive-or a b)))

;;; dimensionality rule
(defmethod region-equal ((a point) (b path))  nil)
(defmethod region-equal ((a point) (b area))  nil)
(defmethod region-equal ((a path)  (b point)) nil)
(defmethod region-equal ((a path)  (b area))  nil)
(defmethod region-equal ((a area)  (b point)) nil)
(defmethod region-equal ((a area)  (b path))  nil)

(defmethod region-equal ((a point) (b point))
  (and (coordinate= (point-x a) (point-x b))
       (coordinate= (point-y a) (point-y b))))

(defmethod region-equal ((xs standard-rectangle-set) (ys standard-rectangle-set))
  ;; Our bands representation is canonic
  (equal (standard-rectangle-set-bands xs)
         (standard-rectangle-set-bands ys)))

(defmethod region-equal ((a standard-rectangle) (b standard-rectangle))
  (multiple-value-bind (x1 y1 x2 y2) (rectangle-edges* a)
    (multiple-value-bind (u1 v1 u2 v2) (rectangle-edges* b)
      (and (coordinate= x1 u1)
           (coordinate= y1 v1)
           (coordinate= x2 u2)
           (coordinate= y2 v2)))))

(defmethod region-equal ((a standard-rectangle) (b path)) nil)
(defmethod region-equal ((a path) (b standard-rectangle)) nil)

(defmethod region-equal ((a standard-line) (b standard-line))
  (or (and (region-equal (line-start-point a) (line-start-point b))
           (region-equal (line-end-point a) (line-end-point b)))
      (and (region-equal (line-start-point a) (line-end-point b))
           (region-equal (line-end-point a) (line-start-point b)))))

