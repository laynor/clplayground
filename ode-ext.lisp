(in-package #:ode.ext)

(ode:init)

;; Object hash
(defparameter *ext-object-hash* (make-hash-table))

(defun ext (obj)
  (let ((h *ext-object-hash*))
    (unless (gethash obj h)
      (setf (gethash obj h) (make-hash-table)))
    (gethash obj h)))


;;; utils
(eval-when (:compile-toplevel)
  (defun string-join (strings &key (separator ""))
    (let ((format-str (concatenate 'string
                                   "~{~A~^"
                                   separator
                                   "~}")))
      (format nil format-str
              strings)))
  )



(defmacro define-ext-slot ((type name) &body body)
  (let* ((get-ext (intern (string-join (list type name)
                                       :separator "-")))
         (key (intern (symbol-name name) "KEYWORD"))
         (set-ext (intern (string-join (list type :set name)
                                       :separator "-"))))
    `(progn
       (defun ,get-ext (obj)
         (gethash ,key (ext obj)))
       (defun ,set-ext (,type ,name)
         (setf (gethash ,key (ext ,type)) ,name)
         ,@body))))

;;; world
;; world constructor
(defun create-world (&key gravity)
  (let ((world (ode:world-create)))
    (when gravity
      (apply #'ode:world-set-gravity world gravity))
    world))

(define-ext-slot (world gravity)
  (apply #'ode:world-set-gravity world gravity))

;;; Mass

;; low level dmass allocator
(defun dmass-alloc ()
  (let ((mass (cffi:foreign-alloc 'ode::dMass)))
    mass))

(defun translate-direction (dir)
  (case dir
    (:x 1)
    (:y 2)
    (:z 3)
    (t (error "invalid direction"))))

(cffi:defcfun (dmass-add "dMassAdd") :void
  "adds m2 to m1 desstructively"
  (m1 ode:dMass)
  (m2 ode:dMass))



(defun mass--set-spec (mass spec)
  (match spec
    ;; sphere
    ((list* :sphere (plist :m m :radius r))
     (ode:mass-set-sphere-total mass m r))
    ((list* :sphere (plist :density d :radius r))
     (ode:mass-set-sphere mass d r))

    ;; capsule
    ((list* :capsule (plist :m m :radius r :direction dir :length l))
     (ode:mass-set-capsule-total mass m (translate-direction dir) r l))
    ((list* :capsule (plist :radius r :density d :direction dir :length l))
     (ode:mass-set-capsule mass d (translate-direction dir) r l))

    ;; cylinder
    ((list* :cylinder (plist :radius r :m m :direction dir :length l))
     (ode:mass-set-cylinder-total mass m (translate-direction dir) r l))
    ((list* :cylinder (plist :radius r :density d :direction dir :length l))
     (ode:mass-set-cylinder mass d (translate-direction dir) r l))

    ;; box
    ((list* :box (plist :dims (list lx ly lz) :m m))
     (ode:mass-set-box-total mass m lx ly lz))
    ((list* :box (plist :dims (list lx ly lz) :density d))
     (ode:mass-set-box mass d lx ly lz))

    ;; zero
    (:zero (ode:mass-set-zero mass))

    ;; union
    ((list* :union specs)
     (ode:mass-set-zero mass)
     (let ((tmp (dmass-alloc)))
       (dolist (s specs)
         (mass--set-spec tmp s)
         (dmass-add mass tmp))
       (cffi:foreign-free tmp))))
  mass)




(define-ext-slot (body mass)
  (mass--set-spec (ode:body-get-mass body) mass))
