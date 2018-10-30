;;;; space-invaders.lisp

(in-package #:space-invaders)

(require :sdl2)
(require :cl-opengl)

(defparameter *game-controllers* nil)
(defparameter *haptic* nil)
(defparameter *input-state* nil)
(defparameter *last-update* nil)
(defparameter *player-state* nil)


(defun init-game-state! ()
  (setf *game-controllers* nil)
  (setf *haptic* nil)
  (setf *player-state* (list :rotation 90.0
                             :position #(0.0 0.0)
                             :accel-start-time nil))
  (setf *input-state* (list :left nil
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
      (let* ((gc (sdl2:game-controller-open i))
             (joy (sdl2:game-controller-get-joystick gc)))
        (push (cons i gc) *game-controllers*)
        (when (sdl2:joystick-is-haptic-p joy)
          (let ((h (sdl2:haptic-open-from-joystick joy)))
            (push (cons i h) *haptic*)
            (sdl2:rumble-init h)))))))

(defun setup-window! (gl-context win)
  (let ((size/2 40))
    (format t "Setting up window/gl.~%")
    (finish-output)
    (sdl2:gl-make-current win gl-context)
    (gl:viewport 0 0 800 600)
    (gl:matrix-mode :projection)
    (gl:ortho (- size/2) size/2 (- size/2) size/2 (- size/2) size/2)
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

(defun rotatez! (theta)
  (gl:rotate theta
             0 0 1))

(defun player-rotation ()
  (getf *player-state* :rotation))

(defun player-position ()
  (getf *player-state* :position))

(defun player-x ()
  (svref (player-position) 0))

(defun player-y ()
  (svref (player-position) 1))

(defun translate! (dx dy)
  (gl:translate dx dy 0))

(defun player-rotation ()
  (getf *player-state* :rotation))

(defun draw-player! ()
  (gl:with-pushed-matrix* (:modelview)
    (translate! (player-x) (player-y))
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

(defun update-player! (last-update curr-update)
  (let* ((dt (time-delta last-update curr-update))
         (angular-velocity 180)
         (speed (player-speed))
         (rot (player-rotation))
         (drot (* angular-velocity dt))
         (ds (* speed dt))
         (dx (* ds (cosd rot)))
         (dy (* ds (sind rot))))
    (cond
      ((input-left?)
       (rotate-player! drot))
      ((input-right?)
       (rotate-player! (- drot))))
    (let* ((accel (if (input-forward? 1) 0))
           (dspeed (* accel dt))
           (dspeedv (vector (* dspeed (cosd rot))
                            (* dspeed (sind rot))))
           (speed (map 'vector (player-speed)
                       (* accel dt))))
      )))


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

(defun basic-test ()
  "The kitchen sink."
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
