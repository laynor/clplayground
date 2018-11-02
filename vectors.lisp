(in-package #:space-invaders)

(defun v+ (&rest vectors)
  (apply #'map 'vector #'+ vectors))

(defun v- (&rest vectors)
  (apply #'map 'vector #'- vectors))

(defun sv* (scalar vector)
  (map 'vector (lambda (el) (* scalar el)) vector))

(defun v* (&rest vectors)
  (apply #'map 'vector #'* vectors))

(defun vdot (v1 v2)
  (reduce #'+ (v* v1 v2)))

(defun vpolar (mag theta)
  (sv* mag
       (vector (cos theta)
               (sin theta))))
