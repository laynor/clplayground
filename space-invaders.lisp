;;;; space-invaders.lisp

(in-package #:space-invaders)

(require :sdl2)
(require :cl-opengl)

(defparameter *game-controllers* nil)
(defparameter *haptic* nil)
(defparameter *input-state* nil)
(defparameter *last-update* nil)
(defparameter *player-state* nil)
(defparameter +size+ 80)

(defun top-left ()
  (let ((d (/ +size+ 2)))
    (vector (- d)
            (- d))))



(defun init-game-state! ()
  (setf *game-controllers* nil
        *haptic* nil
        *player-state* (list :rotation 90.0
                             :position #(0.0 0.0)
                             :accel-start-time nil
                             :speed #(0 0))
        *input-state* (list :left nil
                            :right nil
                            :quit nil))
  nil)

(defun setup-game-controllers! ()
  (format t "Opening game controllers.~%")
  (finish-output)
  ;; open any game controllers
  (dotimes (i (sdl2:joystick-count))
    (when (sdl2:game-controller-p i)
      (format t "Found gamecontroller: ~a~%" (sdl2:game-controller-name-for-index i))
      (let* ((gc  (sdl2:game-controller-open i))
             (joy (sdl2:game-controller-get-joystick gc)))
        (push (cons i gc) *game-controllers*)
        (when (sdl2:joystick-is-haptic-p joy)
          (let ((h (sdl2:haptic-open-from-joystick joy)))
            (push (cons i h) *haptic*)
            (sdl2:rumble-init h)))))))

(defun setup-window! (gl-context win)
  (let ((d (/ +size+ 2)))
    (format t "Setting up window/gl.~%")
    (finish-output)
    (sdl2:gl-make-current win gl-context)
    (gl:viewport 0 0 800 600)
    (gl:matrix-mode :projection)
    (gl:ortho (- d) d (- d) d
              (- d) d)
    (gl:matrix-mode :modelview)
    (gl:load-identity)
    (gl:clear-color 0.01 0.01 0.01 1.0)
    (gl:clear :color-buffer)))

(defun print-sdl-info! ()
  (format t "Using SDL Library Version: ~D.~D.~D~%"
          sdl2-ffi:+sdl-major-version+
          sdl2-ffi:+sdl-minor-version+
          sdl2-ffi:+sdl-patchlevel+)
  (finish-output))

(defun get-haptic (controller-id)
  (cdr (assoc controller-id *haptic*)))

(defun draw-triangle! (size color)
  (let ((offset (/ size 2.0)))
    (gl:begin :triangles)
    (gl:color 1.0 0.0 0.0)
    (gl:vertex 0 offset)
    (apply #'gl:color color)
    (gl:vertex (- offset)  (- offset))
    (gl:vertex offset (- offset))
    (gl:end)))

(defun close-game-controllers! ()
  (format t "Closing opened game controllers.~%")
  (finish-output)
  ;; close any game controllers that were opened as well as any haptics
  (loop :for (i . controller) :in *game-controllers*
        :do (sdl2:game-controller-close controller)
            (sdl2:haptic-close (get-haptic i))))

(defun render-scene! ()
  (draw-player!))

(defun rad->deg (alpha)
  (* (/ alpha
        (float pi alpha))
     180))

(defun rotatez! (theta)
  (gl:rotate (rad->deg theta)
             0 0 1))

(defun player-rotation ()
  (getf *player-state* :rotation))

(defun player-position ()
  (getf *player-state* :position))

(defun player-x ()
  (svref (player-position) 0))

(defun player-y ()
  (svref (player-position) 1))

(defun translate! (ds)
  (gl:translate (elt ds 0) (elt ds 1) 0))

(defun draw-player! ()
  (gl:with-pushed-matrix* (:modelview)
    (translate! (player-position))
    (rotatez!   (player-rotation))
    (rotatez!   -90.0)
    (draw-triangle! 1.0 '(1.0 1.0 1.0))))

(defparameter *keymap*
  '(:scancode-left   :left
    :scancode-right  :right
    :scancode-up     :forward
    :scancode-escape :escape))

(defun translate-input (scancode-sym)
  (getf *keymap* scancode-sym))

(defun log-input! (sym scancode mod-value)
  (format t "Key sym: ~a, code: ~a, mod: ~a~%"
          sym
          scancode
          mod-value))

(defun handle-keyup! (keysym)
  (let* ((scancode  (sdl2:scancode-value keysym))
        ;; (sym       (sdl2:sym-value keysym))
        ;; (mod-value (sdl2:mod-value keysym))
        (scancode-sym (sdl2:scancode-symbol scancode)))
    (setf (getf *input-state* (translate-input scancode-sym)) nil)))

(defun handle-keydown! (keysym)
  (let* ((scancode  (sdl2:scancode-value keysym))
         (sym       (sdl2:sym-value keysym))
         (mod-value (sdl2:mod-value keysym))
         (scancode-sym (sdl2:scancode-symbol scancode)))
    (setf (getf *input-state* (translate-input scancode-sym)) t)
    (log-input! sym scancode mod-value)))

(defun time-delta (t1 t2)
  (or (ignore-errors (/ (- t2 t1)
                        internal-time-units-per-second))
      0))

(defun input-left? ()
  (destructuring-bind (&key left right &allow-other-keys)
      *input-state*
    (and left (not right))))

(defun input-right? ()
  (destructuring-bind (&key left right &allow-other-keys)
      *input-state*
    (and right (not left))))

(defun input-forward? ()
  (getf *input-state* :forward))

(defun rotate-player! (rot &key absolute)
  (if absolute
      (setf (getf *player-state* :rotation) rot)
      (incf (getf *player-state* :rotation) rot)))


(defun translate-player! (x y)
  (setf (getf *player-state* :position)
        (vector (+ (player-x) x)
                (+ (player-y) y))))

(defun sind (deg)
  (sin (* deg (/ (float pi deg) 180))))

(defun cosd (deg)
  (cos (* deg (/ (float pi deg) 180))))

(defun player-accel-start-time ()
  (getf *player-state* :accel-start-time))

(defun player-speed ()
  (getf *player-state* :speed))

(defun (setf player-speed) (new-value)
  (setf (getf *player-state* :speed) new-value))

(defun (setf player-rotation) (new-value)
  (setf (getf *player-state* :rotation) new-value))

(defun (setf player-position) (new-value)
  (setf (getf *player-state* :position) new-value))

(defun player-accel ()
  (getf *player-state* :accel))

(defun (setf player-accel) (new-value)
  (setf (getf *player-state* :accel) new-value))

(init-game-state!)

(defun wrap (pos)
  (let* ((d    (/ +size+ 2))
         (offs (v+ pos (vector d d)))    ; offset from top left
         (x    (- (mod (elt offs 0) +size+)
                  d))
         (y    (- (mod (elt offs 1) +size+)
                  d)))
    (vector x y)))


(defun update-player! (last-update curr-update)
  (let* ((dt    (time-delta last-update curr-update))
         (omega (cond ((input-left?)     pi)
                      ((input-right?) (- pi))
                      (t                  0)))
         (drot  (* omega dt))
         (a     (if (input-forward?) 2 0))
         (dv    (vpolar (* a dt) (player-rotation)))
         (ds    (sv* dt (player-speed))))

    (setf (player-speed)    (v+ (player-speed)    dv)
          (player-rotation) (+  (player-rotation) drot)
          (player-position) (wrap (v+ (player-position) ds)))))


(defun update-state! ()
  (when (getf *input-state* :escape)
    (sdl2:push-event :quit))
  (let ((curr-update (get-internal-real-time)))
    (update-player! *last-update* curr-update)
    (setf *last-update* curr-update)))

(defun event-loop! (win)
  (format t "Beginning main loop.~%")
  (finish-output)
  (sdl2:with-event-loop (:method :poll)
    (:keydown (:keysym keysym)
              (handle-keydown! keysym))

    (:keyup (:keysym keysym)
            (handle-keyup! keysym))

    (:mousemotion (:x x :y y :xrel xrel :yrel yrel :state state)
                  (format t "Mouse motion abs(rel): ~a (~a), ~a (~a)~%Mouse state: ~a~%"
                          x xrel y yrel state))

    (:controlleraxismotion (:which controller-id :axis axis-id :value value)
                           (format t "Controller axis motion: Controller: ~a, Axis: ~a, Value: ~a~%"
                                   controller-id axis-id value))

    (:controllerbuttondown (:which controller-id)
                           (when-let ((h (get-haptic controller-id)))
                             (sdl2:rumble-play h 1.0 100)))

    (:idle ()
           (gl:clear :color-buffer)
           (update-state!)
           (render-scene!)
           (gl:flush)
           (sdl2:gl-swap-window win))

    (:quit () t)))

(defparameter *world* nil)

(defun setup-physics! ()
  (ode:init)
  (setf *world* (ode:world-create)))

(defun basic-test ()
  "The kitchen sink."
  (setup-physics!)
  (sdl2:with-init (:everything)
    (print-sdl-info!)
    (sdl2:with-window (win :flags '(:shown :opengl))
      (sdl2:with-gl-context (gl-context win)
        ;; basic window/gl setup
        (setup-window! gl-context win)
        (init-game-state!)
        (setup-game-controllers!)
        ;; main loop
        (event-loop! win)
        (close-game-controllers!)))))
