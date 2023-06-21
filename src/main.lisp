(defpackage clip8
  (:use :cl))
(in-package :clip8)

(require :sdl2)
(require :alexandria )
(require :cl-interpol)
(require :adopt)

;; http://devernay.free.fr/hacks/chip8/C8TECH10.HTM#3.0
;; https://github.com/Timendus/chip8-test-suite

;;(ql:quickload :sdl2)
;;(ql:quickload :alexandria)
;;(ql:quickload :cl-interpol)
;;(ql:quickload :adopt)

(cl-interpol:enable-interpol-syntax)
(setf cl-interpol:*interpolate-format-directives* t)

(defparameter *screen-scale* 20)
(defparameter *screen-black* #xACC7B4FF)
(defparameter *screen-white* #x331B3FFF)

(defparameter *type* :chip8) ;; :chip8 :schip

(defparameter *cpu-sync-clocks* 16) ;; Is evenly divisible with 640 Hz
(defparameter *timer-cpu-period* 10) ;; Clock cycles per timer tic
(defparameter *cpu-hz* 640)
(defparameter *brk-points* '())

(defconstant +memory-size+ 4096)
(defconstant +num-registers+ 16)
(defconstant +stack-size+ 16)
(defconstant +pc-start+ #x200)
(defconstant +screen-width+ 64)
(defconstant +screen-height+ 32)
(defconstant +pc-increment+ 2)
(defconstant +vf-index+ #xf)
(defconstant +sprite-width+ 8)

(defparameter +debug+ nil)

(defconstant *debug*-key-break/step+ :scancode-space)
(defconstant *debug*-key-continue+ :scancode-return)

(defparameter +key-map+
  '(:scancode-x
    :scancode-1
    :scancode-2
    :scancode-3
    :scancode-q
    :scancode-w
    :scancode-e
    :scancode-a
    :scancode-s
    :scancode-d
    :scancode-z
    :scancode-c
    :scancode-4
    :scancode-r
    :scancode-f
    :scancode-v))

(defparameter +font-set+
  '(#xF0 #x90 #x90 #x90 #xF0 ;; 0
    #x20 #x60 #x20 #x20 #x70 ;; 1
    #xF0 #x10 #xF0 #x80 #xF0 ;; 2
    #xF0 #x10 #xF0 #x10 #xF0 ;; 3
    #x90 #x90 #xF0 #x10 #x10 ;; 4
    #xF0 #x80 #xF0 #x10 #xF0 ;; 5
    #xF0 #x80 #xF0 #x90 #xF0 ;; 6
    #xF0 #x10 #x20 #x40 #x40 ;; 7
    #xF0 #x90 #xF0 #x90 #xF0 ;; 8
    #xF0 #x90 #xF0 #x10 #xF0 ;; 9
    #xF0 #x90 #xF0 #x90 #x90 ;; A
    #xE0 #x90 #xE0 #x90 #xE0 ;; B
    #xF0 #x80 #x80 #x80 #xF0 ;; C
    #xE0 #x90 #x90 #x90 #xE0 ;; D
    #xF0 #x80 #xF0 #x80 #xF0 ;; E
    #xF0 #x80 #xF0 #x80 #x80 ;; F
    ))

(defconstant +font-set-start+ #x0000)

(defstruct chip
  (running nil :type boolean)
  (redraw t :type boolean)
  (cycle-count 0 :type integer)
  (sync-cycle-time 0 :type integer)
  (pc +pc-start+ :type (unsigned-byte 16))
  (registers (make-array +num-registers+
                         :element-type '(unsigned-byte 8)))
  (I 0 :type (unsigned-byte 16))
  (stack (make-array +stack-size+
                     :element-type '(unsigned-byte 16)))
  (stack-pointer 0 :type (unsigned-byte 16))
  (memory (make-array +memory-size+
                      :element-type '(unsigned-byte 8)))
  (screen (make-array `(,+screen-height+ ,+screen-width+)
                      :element-type 'boolean
                      :initial-element nil))
  (delay-timer 0 :type (unsigned-byte 8))
  (sound-timer 0 :type (unsigned-byte 8))
  (keys (make-array (length +key-map+)
                    :element-type 'boolean
                    :initial-element nil))
  (waiting-for-key nil :type boolean)
  (waiting-for-key-register 0 :type integer))

(defparameter *chip* (make-chip)) ;; Usefull for repl action

(defmacro with-chip (chip &body body)
  `(with-accessors ((running chip-running)
                    (cycle-count chip-cycle-count)
                    (sync-cycle-time chip-sync-cycle-time)
                    (redraw chip-redraw)
                    (pc chip-pc)
                    (registers chip-registers)
                    (I chip-I)
                    (stack chip-stack)
                    (stack-pointer chip-stack-pointer)
                    (memory chip-memory)
                    (screen chip-screen)
                    (delay-timer chip-delay-timer)
                    (sound-timer chip-sound-timer)
                    (keys chip-keys)
                    (waiting-for-key chip-waiting-for-key)
                    (waiting-for-key-register chip-waiting-for-key-register))
       ,chip
     ,@body))


(defmacro definstruction (name doc &body body)
  `(progn
    (declaim (ftype (function (chip (unsigned-byte 16)) null) ,name))
    (defun ,name (chip op)
      (declare (ignorable op))
      (with-chip chip
        (symbol-macrolet
            ((nnn (logand op #x0FFF))
             (kk (logand op #x00FF))
             (n (logand op #x000F))
             (y (logand (ash op -4) #x000F))
             (x (logand (ash op -8) #x000F))
             (vx (aref registers x))
             (vy (aref registers y))
             (vf (aref registers ,+vf-index+)))
          (when +debug+
            (format T "~5d | [0x~4x] ~s~%" (- pc +pc-increment+) op ,doc))
          ,@body))
      nil)))

(definstruction cls
  "00E0: Clear screen"
  (dotimes (y +screen-height+)
    (dotimes (x +screen-width+)
      (setf (aref screen y x) nil))))

(definstruction ret
    "00EE: Return from subroutine"
  (setf stack-pointer (1- stack-pointer))
  (setf pc (aref stack stack-pointer)))

(definstruction jmp
  #?"1nnn: Jump to address ~x(nnn)"
  (setf pc nnn))

(definstruction call
  #?"2nnn: Call subroutine at ~x(nnn)"
  (setf (aref stack stack-pointer) pc)
  (setf stack-pointer (1+ stack-pointer))
  (setf pc nnn))

(definstruction se-vx-kk
  #?"3xkk: Skip next instruction if V~x(x) = ~x(kk)"
  (when (= vx kk)
    (setq pc (+ pc +pc-increment+))))

(definstruction sne-vx-kk
  #?"4xkk: Skip next instruction if V~x(x) != ~x(kk)"
  (unless (= vx kk)
    (setq pc (+ pc +pc-increment+))))

(definstruction se-vx-vy
  #?"5xy0: Skip next instruction if V~x(x) = V~x(y)"
  (when (= vx vy)
    (setq pc (+ pc +pc-increment+))))

(definstruction ld-vx-kk
  #?"6xkk: Set V~x(x) to ~d(kk)"
  (setf vx kk))

(definstruction add-vx-kk
  #?"7xkk: Set V~x(x) = ~x(vx) + ~x(kk)"
  (setf vx (logand (+ vx kk) #xFF)))

(definstruction ld-vx-yy
  #?"8xy0: Set V~x(x) = V~x(y)"
  (setf vx vy))

(definstruction or-vx-yy
  #?"8xy1: Set V~x(x) = V~x(x) | V~x(y)"
  (setf vx (logior vx vy))
  (when (member *type* '(:chip8))
    (setf vf 0)))

(definstruction and-vx-yy
  #?"8xy2: Set V~x(x) = V~x(x) & V~x(y)"
  (setf vx (logand vx vy))
  (when (member *type* '(:chip8))
    (setf vf 0)))

(definstruction xor-vx-yy
  #?"8xy3: Set V~x(x) = V~x(x) ^ V~x(y)"
  (setf vx (logxor vx vy))
  (when (member *type* '(:chip8))
    (setf vf 0)))

(definstruction add-vx-yy
  #?"8xy4: Set V~x(x) = V~x(x) + V~x(y), set VF = carry"
  (let ((sum (+ vx vy)))
    (setf vx (logand sum #xFF))
    (setf vf (if (> sum #xFF) 1 0))))

(definstruction sub-vx-yy
  #?"8xy5: Set V~x(x) = V~x(x) - V~x(y), set VF = not borrow"
  (let ((diff (- vx vy)))
    (setf vx (logand diff #xFF))
    (setf vf (if (> diff 0) 1 0))))

(definstruction shr-vx
  #?"8xy6: V~x(x) = V~x(x) >> 1"
  (let ((new-vf (logand vx #x01)))
    (setf vx (logand (ash vx -1) #xFF))
    (setf vf new-vf)))

(definstruction sub-vy-yx
  #?"8xy7: Set V~x(x) = V~x(y) - V~x(x), set VF = not borrow"
  (let ((diff (- vy vx)))
    (setf vx (logand diff #xFF))
    (setf vf (if (> diff 0) 1 0))))

(definstruction shl-vx
  #?"8xyE: V~x(x) = V~x(x) << 1"
  (let ((new-vf (if (/= (logand vx #x80) 0) 1 0)))
    (setf vx (logand (ash vx 1) #xFF))
    (setf vf new-vf)))

(definstruction sen-vx-vy
  #?"9xy0: Skip next instruction if V~x(x) = V~x(y)"
  (unless (= vx vy)
    (setq pc (+ pc +pc-increment+))))

(definstruction ld-i
  #?"Annn: Set I register to ~d(nnn)"
  (setf I nnn))

(definstruction jmp-nnn
  #?"Bnnn: Jump to ~x(nnn) + v0/~x(x)"
  (let ((register (case *type*
                        (:schip vx)
                        (t (aref registers 0)))))
    (setf pc (+ nnn register))))

(definstruction rnd
  #?"Cxkk: Set V~x(x) = random byte & ~x(kk)"
  (setf vx (logand (random #x1FF) kk)))

(definstruction drw
  #?"Dxyn: Display ~d(n)-byte sprite starting at memory location ~x(I) at (V~x(x)=~d(vx), V~x(y)=~d(vy))"
  (setf vf 1)
  ;; TODO: (+ vx x-coord) and (+ vy y-cord) should mod the screen width/height
  ;;       if compatability with chip8 and schip
  (dotimes (y-cord n)
    (dotimes (x-cord +sprite-width+)
      ;; Clip sprites
      (when (and (> (array-dimension screen 0) (+ vy y-cord))
                 (> (array-dimension screen 1) (+ vx x-cord)))
        (let ((screen-pixel (aref screen (+ vy y-cord) (+ vx x-cord)))
              (memory-pixel (/= (logand (aref memory (+ I y-cord))
                                        (ash #x80 (- x-cord)))
                                0)))
          (when memory-pixel
            (setf redraw t) ;; TODO: might not be needed
            (when screen-pixel
              (setf vf 1))
            (setf (aref screen (+ vy y-cord) (+ vx x-cord))
                  (not screen-pixel))))))))

(definstruction skp
    #?"Ex9E: Skip next instruction if key with the value of ~d(vx) is pressed"
  (when (aref keys vx)
    (setq pc (+ pc +pc-increment+))))

(definstruction skpn
    #?"ExA1: Skip next instruction if key with the value of ~d(vx) is not pressed"
  (unless (aref keys vx)
    (setq pc (+ pc +pc-increment+))))

(definstruction ld-vx-dl
    #?"Fx07: Set V~x(x) = ~d(delay-timer) delay timer value"
  (setf vx delay-timer))

(definstruction ld-key
    #?"Fx0A: Wait for a key press, store the value of the key in V~x(x)"
  (setf waiting-for-key t)
  (setf waiting-for-key-register x))

(definstruction ld-dt
    #?"Fx15: Set delay timer = ~d(vx)"
  (setf delay-timer vx))

(definstruction ld-st
    #?"Fx18: Set sound timer = ~d(vx)"
  (setf sound-timer vx))

(definstruction add-i
    #?"Fx1E: Set I = ~d(I) + ~d(vx)"
  (setf I (+ I vx)))

(definstruction ld-f
    #?"Fx29: Set I = location of sprite for digit ~x(vx)"
  (setf I (+ +font-set-start+
             (* +sprite-width+ vx))))

(definstruction ld-b
    #?"Fx33: Store BCD representation of ~d(vx) in memory locations ~x(I), ~x(I)+1, and ~x(I)+2"
  (loop for position in '(2 1 0)
        for idx in '(0 1 2)
        do (setf (aref memory (+ I idx))
                 (mod (floor vx (expt 10 position)) 10))))

(definstruction ld-regs
    #?"Fx55: Store registers V0 through V~x(x) in memory starting at location ~x(I)"
  (dotimes (idx (1+ x))
    (setf (aref memory (+ I idx))
          (aref registers idx)))
  (when (member *type* '(:chip8))
    (setf I (1+ x))))

(definstruction ld-mem
    #?"Fx65: Load registers V0 through V~x(x) from memory starting at location ~x(I)"
  (dotimes (idx (1+ x))
    (setf (aref registers idx)
          (aref memory (+ I idx))))
  (when (member *type* '(:chip8))
    (setf I (1+ x))))

(defun create-texture-buffer (chip)
  (let* ((dim (array-dimensions (chip-screen chip)))
         (buffer (make-array (apply '* dim)
                             :element-type '(unsigned-byte 32))))
    (dotimes (y +screen-height+)
      (dotimes (x +screen-width+)
        (setf (aref buffer (+ (* +screen-width+ y) x))
              (if (aref (chip-screen chip) y x)
                  *screen-black*
                  *screen-white*))))
    buffer))

(defun render-chip (chip renderer texture &optional rect)
  (with-chip chip
    (when redraw
      ;; TODO: this copy copy is bugging me
      (let* ((buffer (create-texture-buffer chip)))
        (cffi:with-foreign-array (buffer-ptr buffer `(:array :uint32 ,(array-total-size buffer)))
          (sdl2:update-texture texture rect buffer-ptr (* +screen-width+ 4))
          (sdl2:render-clear renderer)
          (sdl2:render-copy renderer texture)
          (sdl2:render-present renderer)))
      (setf redraw nil))))

(defun make-texture (renderer)
  (sdl2:create-texture renderer
                       sdl2-ffi:+sdl-pixelformat-rgba8888+
                       sdl2-ffi:+sdl-textureaccess-target+
                       +screen-width+ +screen-height+))

(defmacro with-window-renderer ((window renderer) &body body)
  `(sdl2:with-init (:video)
     (sdl2:with-window (,window
                        :title "Clip8"
                        :w (* *screen-scale* +screen-width+)
                        :h (* *screen-scale* +screen-height+)
                        :flags '(:shown))
       (sdl2:with-renderer (,renderer ,window :index -1 :flags '(:accelerated))
         ,@body))))

(defun handle-keysym (chip keysym &optional keydown)
  (with-chip chip
    (let* ((scancode (sdl2:scancode-value keysym))
           (key-index (position scancode +key-map+ :test 'sdl2:scancode=)))
      (when key-index
        (when (and waiting-for-key (not keydown))
          (setf (aref registers waiting-for-key-register)
                (logand key-index #xFF))
          (setf waiting-for-key nil))
        (setf (aref keys key-index)
              keydown))
      (when (not keydown)
        ;; Debug keys
        (cond
          ((and running (sdl2:scancode= scancode *debug*-key-break/step+))
           (format T "Stopping execution...~%")
           (setf +debug+ t)
           (setf running nil))
          ((and (not running) (sdl2:scancode= scancode *debug*-key-break/step+))
           (setf running t)
           (chip-cycle chip)
           (setf running nil))
          ((sdl2:scancode= scancode *debug*-key-continue+)
           (format T "Starting execution...~%")
           (setf +debug+ nil)
           (setf running t)))))))

(defun code->instruction (code)
  (case (logand code #xF000)
    (#x0000
     (case code
       (#x00E0 'cls)
       (#x00EE 'ret)))
    (#x1000 'jmp)
    (#x2000 'call)
    (#x3000 'se-vx-kk)
    (#x4000 'sne-vx-kk)
    (#x5000 'se-vx-vy)
    (#x6000 'ld-vx-kk)
    (#x7000 'add-vx-kk)
    (#x8000 (case (logand code #xF00F)
              (#x8000 'ld-vx-yy)
              (#x8001 'or-vx-yy)
              (#x8002 'and-vx-yy)
              (#x8003 'xor-vx-yy)
              (#x8004 'add-vx-yy)
              (#x8005 'sub-vx-yy)
              (#x8006 'shr-vx)
              (#x8007 'sub-vy-yx)
              (#x800E 'shl-vx)))
    (#x9000 'sen-vx-vy)
    (#xA000 'ld-i)
    (#xB000 'jmp-nnn)
    (#xC000 'rnd)
    (#xD000 'drw)
    (#xE000 (case (logand code #xF0FF)
              (#xE09E 'skp)
              (#xE0A1 'skpn)))
    (#xF000 (case (logand code #xF0FF)
              (#xF007 'ld-vx-dl)
              (#xF00A 'ld-key)
              (#xF015 'ld-dt)
              (#xF018 'ld-st)
              (#xF01E 'add-i)
              (#xF029 'ld-f)
              (#xF033 'ld-b)
              (#xF055 'ld-regs)
              (#xF065 'ld-mem)))))

(defun chip-op-code (chip)
  (with-chip chip
    (+ (ash (aref memory pc) 8)
       (aref memory (1+ pc)))))

(defun chip-skip-sync-cycle-p (chip)
  (with-chip chip
    (and
     ;; Is sync cycle?
     (zerop (mod cycle-count *cpu-sync-clocks*))
     ;; Have enough time transpired to sync clock speed with real time
     (< (- (sdl2:get-ticks) sync-cycle-time)
        (* (/ 1000 *cpu-hz*) *cpu-sync-clocks*)))))

(defun chip-tick-clocks (chip)
  (with-chip chip
    (when (zerop (mod cycle-count *timer-cpu-period*))
      (setf delay-timer (max (1- delay-timer) 0)
            sound-timer (max (1- sound-timer) 0)))))

(defun chip-cycle (chip)
  (with-chip chip
    (when running
      (unless (chip-skip-sync-cycle-p chip)
        ;; If start of sync window set start sync time
        (when (zerop (mod cycle-count *cpu-sync-clocks*))
          (setf sync-cycle-time (sdl2:get-ticks)))
        ;; Always tick clocks
        (chip-tick-clocks chip)
        (let* ((start-pc pc)
               (code (chip-op-code chip))
               (instruction (code->instruction code)))
          (when (and instruction
                     (not waiting-for-key))
            ;; Inc pc before instruction
            (setf pc (+ pc +pc-increment+))
            ;; Call instruction
            (funcall instruction chip code)
            ;; Break after execution
            (when (member start-pc *brk-points*)
              (setf running nil))
            ;; Stop execution at infinite loop
            (when (= start-pc pc)
              (setf running nil))))
        ;; Increment cycle count
        (setf cycle-count (1+ cycle-count))))))

(defun load-rom (chip path)
  (with-chip chip
    (fill memory 0)
    (fill registers 0)
    (fill stack 0)
    (fill keys nil)
    (setf running t
          pc +pc-start+
          cycle-count 0
          delay-timer 0
          sound-timer 0
          waiting-for-key nil)
    (replace memory (alexandria:read-file-into-byte-vector path)
             :start1 +pc-start+)
    (replace memory +font-set+
             :start1 +font-set-start+))
  (values))

(defun run (path &optional start-in-debug)
  (with-window-renderer (window renderer)
    (let* ((texture (make-texture renderer))
           (rect (sdl2:make-rect 0 0
                                 (* *screen-scale* +screen-width+)
                                 (* *screen-scale* +screen-width+)))
           (chip (make-chip)))
      (setf *chip* chip)
      (load-rom chip path)
      (when start-in-debug
        (setf (chip-running chip) nil))
      (sdl2:pump-events)
      (sdl2:with-event-loop (:method :poll)
        (:quit () t)
        (:keydown (:keysym keysym)
                  (handle-keysym chip keysym t))
        (:keyup (:keysym keysym)
                (handle-keysym chip keysym nil))
        (:idle ()
               (chip-cycle chip)
               (render-chip chip renderer texture rect)))
      (sdl2:pump-events))))

(defparameter *option-debug*
  (adopt:make-option 'debug
    :long "debug"
    :short #\d
    :help "Start in debug"
    :reduce (constantly t)))

(defparameter *option-help*
  (adopt:make-option 'help
    :long "help"
    :short #\h
    :help "Display help and exit."
    :reduce (constantly t)))

(defparameter *ui*
  (adopt:make-interface
    :name "clip8"
    :summary "non-working emulator for chip8 + famility"
    :usage "[OPTIONS] [ROM FILE]"
    :help "Emulate rom"
    :contents (list
                *option-debug*
                *option-help*)))

(defun main ()
  (handler-case
      (multiple-value-bind (files options) (adopt:parse-options *ui*)
        (when (gethash 'help options)
          (adopt:print-help-and-exit *ui*)
          (adopt:exit))
        (run files
             (gethash 'debug options)))
    (error (c)
      (adopt:print-error-and-exit c))))
