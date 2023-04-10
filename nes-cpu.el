;; -*- lexical-binding: t -*-

(eval-when-compile (require 'cl-lib))

(require 'nes-dma)
(require 'nes-instruction)
(require 'nes-interrupt)
(require 'nes-keypad)
(require 'nes-ppu)
(require 'nes-util)

;; https://wiki.nesdev.com/w/index.php/CPU_registers
(cl-defstruct (nes/cpu-register
               (:conc-name nes/cpu-register->))
  (acc 0)
  (idx-x 0)
  (idx-y 0)
  (pc 0)
  (sp #x01fd)

  ;; status register
  (sr-carry nil)
  (sr-zero nil)
  (sr-interrupt t)
  (sr-decimal nil)
  (sr-break t)
  (sr-reserved t)
  (sr-overflow nil)
  (sr-negative nil))

(cl-defstruct (nes/cpu-bus
            (:conc-name nes/cpu-bus->))
  (ram nil)
  (prg-rom (lambda (ignored))))

(cl-defstruct (nes/cpu
            (:conc-name nes/cpu->))
  (cycles 0)
  (ppu nil)
  (dma nil)
  (keypad nil)
  (bus (make-nes/cpu-bus))
  (register (make-nes/cpu-register))
  (interrupt nil))

(defun nes/cpu--bus-read (cpu addr)
  "Read byte at ADDR of CPU bus."
  (let ((b (nes/cpu->bus cpu))
        (ppu (nes/cpu->ppu cpu)))
    (cond
     ((< addr #x0800) (aref (nes/cpu-bus->ram b) addr))
     ((< addr #x2000) (nes/cpu--bus-read cpu (- addr #x0800)))
     ((< addr #x4000) (nes/ppu-read ppu (+ (mod addr #x0008) #x2000)))
     ((eq addr #x4016) (nes/keypad-read (nes/cpu->keypad cpu))) ; 1P
     ;; ((eq addr #x4017) (nes/keypad-read (nes/cpu->keypad c))) ;; 2P
     ((>= addr #x8000) (funcall (nes/cpu-bus->prg-rom b) addr))
     (t 0))))

(defun nes/cpu--bus-write (cpu addr data)
  "Write byte DATA to ADDR of CPU bus."
  (let ((b (nes/cpu->bus cpu))
        (ppu (nes/cpu->ppu cpu)))
    (cond
     ((< addr #x2000) (aset (nes/cpu-bus->ram b) (% addr #x0800) data))
     ((< addr #x4000) (nes/ppu-write ppu (+ (% addr #x0008) #x2000) data))
     ;; ((< addr #x4014) ( ... )) ;; APU
     ((eq addr #x4014) (nes/dma-request-transfer (nes/cpu->dma cpu) data))
     ((eq addr #x4016) (nes/keypad-write (nes/cpu->keypad cpu) data))
     ;; ((eq addr #x4017) (nes/keypad-write (nes/cpu->keypad c) data))  ;; 2P
     )))

(defun nes/cpu-read (cpu addr &optional size)
  "Read SIZE data at ADDR of CPU bus.

SIZE can be :byte or :word"
  (setq size (or size :byte))
  (setq addr (logand addr #xffff))
  (if (eq size :word)
      (logior
       (logand (nes/cpu--bus-read cpu addr) #xFF)
       (logand (ash (nes/cpu--bus-read cpu (1+ addr)) 8) #xFF00))
    (logand (nes/cpu--bus-read cpu addr) #xFF)))

(defun nes/cpu-write (cpu addr data)
  "Write DATA to ADDR of CPU bus."
  (nes/cpu--bus-write cpu addr data))

(defun nes/cpu-push (cpu data)
  "Push DATA to stack of CPU."
  (let* ((register (nes/cpu->register cpu))
         (addr (nes/cpu-register->sp register)))
    (nes/cpu-write cpu (logior #x100 (logand addr #x0ff)) data)
    (setf (nes/cpu-register->sp register) (1- addr))))

(defun nes/cpu-pull (cpu)
  "Pull byte from stack of CPU."
  (let* ((register (nes/cpu->register cpu))
         (addr (1+ (nes/cpu-register->sp register))))
    (setf (nes/cpu-register->sp register) addr)
    (nes/cpu-read cpu (logior #x100 (logand addr #x0ff)))))

(defun nes/cpu-status-register (cpu)
  "Return status register of CPU."
  (let ((r (nes/cpu->register cpu)))
    (logior (ash (if (nes/cpu-register->sr-negative r)  1 0) 7)
            (ash (if (nes/cpu-register->sr-overflow r)  1 0) 6)
            (ash (if (nes/cpu-register->sr-reserved r)  1 0) 5)
            (ash (if (nes/cpu-register->sr-break r)     1 0) 4)
            (ash (if (nes/cpu-register->sr-decimal r)   1 0) 3)
            (ash (if (nes/cpu-register->sr-interrupt r) 1 0) 2)
            (ash (if (nes/cpu-register->sr-zero r)      1 0) 1)
            (ash (if (nes/cpu-register->sr-carry r)     1 0) 0))))

(defun nes/cpu-push-status-register (cpu)
  "Push status register to stack of CPU."
  (nes/cpu-push cpu (nes/cpu-status-register cpu)))

(defun nes/cpu-pull-status-register (cpu)
  "Pull byte from stack and use it to set status register of CPU."
  (let ((data (nes/cpu-pull cpu))
        (r (nes/cpu->register cpu)))
    (setf (nes/cpu-register->sr-negative r)  (nes--logbitp 7 data))
    (setf (nes/cpu-register->sr-overflow r)  (nes--logbitp 6 data))
    (setf (nes/cpu-register->sr-reserved r)  (nes--logbitp 5 data))
    (setf (nes/cpu-register->sr-break r)     (nes--logbitp 4 data))
    (setf (nes/cpu-register->sr-decimal r)   (nes--logbitp 3 data))
    (setf (nes/cpu-register->sr-interrupt r) (nes--logbitp 2 data))
    (setf (nes/cpu-register->sr-zero r)      (nes--logbitp 1 data))
    (setf (nes/cpu-register->sr-carry r)     (nes--logbitp 0 data))))

(defun nes/cpu-reset (cpu)
  "Reset CPU."
  (let ((r (nes/cpu->register cpu)))
    (setf (nes/cpu-register->sp r) #xFD)
    (setf (nes/cpu-register->pc r) (nes/cpu-read cpu #xFFFC :word))
    (setf (nes/cpu-register->sr-carry r) nil)
    (setf (nes/cpu-register->sr-zero r) nil)
    (setf (nes/cpu-register->sr-interrupt r) t)
    (setf (nes/cpu-register->sr-decimal r) nil)
    (setf (nes/cpu-register->sr-break r) t)
    (setf (nes/cpu-register->sr-reserved r) t)
    (setf (nes/cpu-register->sr-overflow r) nil)
    (setf (nes/cpu-register->sr-negative r) nil)))

(defun nes/cpu-nmi (c)
  (let ((register (nes/cpu->register c)))
    (nes/interrupt-deassert-nmi (nes/cpu->interrupt c))
    (setf (nes/cpu-register->sr-break register) nil)
    (nes/cpu-push c (logand #xFF (ash (nes/cpu-register->pc register) -8)))
    (nes/cpu-push c (logand #xFF (nes/cpu-register->pc register)))
    (nes/cpu-push-status-register c)
    (setf (nes/cpu-register->sr-interrupt register) t)
    (setf (nes/cpu-register->pc register) (nes/cpu-read c #xFFFA :word))))

(defun nes/cpu-irq (c)
  (let ((register (nes/cpu->register c)))
    (nes/cpu-push c (logand #xff (ash (nes/cpu-register->pc register) -8)))
    (nes/cpu-push c (logand #xff (nes/cpu-register->pc register)))
    (nes/cpu-push-status-register c)
    (setf (nes/cpu-register->sr-interrupt register) t)
    (setf (nes/cpu-register->pc register) (nes/cpu-read c #xfffe :word))))

(defun nes/cpu--get-instruction-operand-and-cycle (cpu mode)
  "Return current instruction operand given an addressing MODE of CPU."
  (let ((register (nes/cpu->register cpu))
        base-addr
        data-addr
        addr
        idx-x
        idx-y
        cycle)
    ;; Sorted, most used addressing mode first, according to
    ;; http://blargg.8bitalley.com/nes-emu/6502.html
    (cond
     ;; Zero Page
     ;; https://www.pagetable.com/c64ref/6502/?tab=3#r8
     ((eq mode :zero-page)
      (cons (nes/cpu--fetch cpu) 0))
     ;; Absolute
     ;; https://www.pagetable.com/c64ref/6502/?tab=3#a16
     ((eq mode :absolute)
      (cons (nes/cpu--fetch cpu :word) 0))
     ;; Immediate
     ;; https://www.pagetable.com/c64ref/6502/?tab=3##d8
     ((eq mode :immediate)
      (cons (nes/cpu--fetch cpu) 0))
     ;; X-Indexed Absolute
     ;; https://www.pagetable.com/c64ref/6502/?tab=3#a16,X
     ((eq mode :absolute-x)
      (setq addr (nes/cpu--fetch cpu :word)
            idx-x (nes/cpu-register->idx-x register)
            cycle (if (eq (logand addr #xFF00) (logand (+ addr idx-x) #xFF00))
                      0
                    1))
      (cons (logand (+ addr idx-x) #xFFFF) cycle))
     ;; Accumulator
     ;; https://www.pagetable.com/c64ref/6502/?tab=3#A
     ((eq mode :accumulator)
      '(nil . 0))
     ;; X-Indexed Zero Page
     ;; https://www.pagetable.com/c64ref/6502/?tab=3#a8,X
     ((eq mode :zero-page-x)
      (cons (logand (+ (nes/cpu--fetch cpu)
                       (nes/cpu-register->idx-x register))
                    #xFF)
            0))
     ;; Y-Indexed Zero Page
     ;; https://www.pagetable.com/c64ref/6502/?tab=3#a8,Y
     ((eq mode :zero-page-y)
      (cons (logand (+ (nes/cpu--fetch cpu)
                       (nes/cpu-register->idx-y register))
                    #xFF)
            0))
     ;; Implied
     ;; https://www.pagetable.com/c64ref/6502/?tab=3#-
     ((eq mode :implied)
      '(nil . 0))
     ;; Relative
     ;; https://www.pagetable.com/c64ref/6502/?tab=3#r8
     ((eq mode :relative)
      (setq base-addr (nes/cpu--fetch cpu)
            addr (if (< base-addr #x80)
                     (+ base-addr (nes/cpu-register->pc register))
                   (- (+ base-addr (nes/cpu-register->pc register)) 256))
            cycle (if (not (eq (logand addr #xFF00) (logand (nes/cpu-register->pc register) #xFF00))) 1 0))
      (cons addr cycle))
     ;; Y-Indexed Absolute
     ;; https://www.pagetable.com/c64ref/6502/?tab=3#a16,Y
     ((eq mode :absolute-y)
      (setq addr (nes/cpu--fetch cpu :word)
            idx-y (nes/cpu-register->idx-y register)
            cycle (if (eq (logand addr #xFF00) (logand (+ addr idx-y) #xFF00))
                      0 1))
      (cons (logand (+ addr idx-y) #xFFFF) cycle))
     ;; X-Indexed Zero Page Indirect
     ;; https://www.pagetable.com/c64ref/6502/?tab=3#(a8,X)
     ((eq mode :pre-indexed-indirect)
      (setq base-addr (logand (+ (nes/cpu--fetch cpu) (nes/cpu-register->idx-x register))
                              #xFF)
            addr (logand (+ (nes/cpu-read cpu base-addr)
                            (ash (nes/cpu-read cpu (logand (1+ base-addr) #xFF)) 8))
                         #xFFFF)
            ;; cycle (if (/= (logand addr #xFF00) (logand base-addr #xFF00)) 1 0)
            )
      (cons addr 0))
     ;; Zero Page Indirect Y-Indexed
     ;; https://www.pagetable.com/c64ref/6502/?tab=3#(a8),Y
     ((eq mode :post-indexed-indirect)
      (setq data-addr (nes/cpu--fetch cpu)
            base-addr (+ (nes/cpu-read cpu data-addr)
                         (ash (nes/cpu-read cpu (logand (1+ data-addr) #xFF)) 8))
            addr (logand (+ base-addr (nes/cpu-register->idx-y register)) #xFFFF)
            cycle (if (/= (logand addr #xFF00) (logand base-addr #xFF00)) 1 0))
      (cons addr cycle))
     ;; Absolute Indirect
     ;; https://www.pagetable.com/c64ref/6502/?tab=3#(a16)
     ((eq mode :indirect-absolute)
      (setq data-addr (nes/cpu--fetch cpu :word))
      (cons (logand (+ (nes/cpu-read cpu data-addr)
                       (ash (nes/cpu-read cpu (logior (logand data-addr #xFF00)
                                                      (logand (1+ (logand data-addr #xFF)) #xFF)))
                            8))
                    #xFFFF)
            0)))))

(defun nes/cpu--fetch (cpu &optional size)
  "Read SIZE data at program counter of CPU bus.

SIZE can be :byte or :word"
  (let ((size (or size :byte))
        (addr (nes/cpu-register->pc (nes/cpu->register cpu))))
    (cl-incf (nes/cpu-register->pc (nes/cpu->register cpu)) (if (eq size :word) 2 1))
    (nes/cpu-read cpu addr size)))

(defun nes/cpu-set-working-ram (cpu ram)
  (setf (nes/cpu-bus->ram (nes/cpu->bus cpu)) ram))

(defun nes/cpu-set-program-rom (cpu rom-func)
  (setf (nes/cpu-bus->prg-rom (nes/cpu->bus cpu)) rom-func))

;; TODO: make nes/cpu->cycles accumulate all the cycles with as 16 bit counter
(defun nes/cpu-step (cpu)
  "Make CPU run next instruction."
  (when (nes/interrupt->nmi (nes/cpu->interrupt cpu))
    (nes/cpu-nmi cpu))
  (nes/interrupt-clear (nes/cpu->interrupt cpu))
  (let* ((opcode (nes/cpu--fetch cpu))
         (instruction (aref nes/instruction:MAP opcode))
         (instruction-mode (nes/instruction->mode instruction))
         (instruction-name (nes/instruction->name instruction))
         (operand-and-cycle (nes/cpu--get-instruction-operand-and-cycle cpu instruction-mode))
         (operand (car operand-and-cycle))
         (instruction-cycles (aref nes/instruction:CYCLES opcode))
         (penalty-cycles (cdr operand-and-cycle)))
    (setf (nes/cpu->cycles cpu) 0)
    (funcall (nes/instruction->func instruction) cpu operand instruction-mode)
    (+ (nes/cpu->cycles cpu)
       instruction-cycles
       ;; NOTE: we are going to pay the extra cycle for crossing memory page
       ;; only if the op did something aka changed the cycle aka ex. jumped
       ;; TODO: make nes/instruction-* take cycles parameter
       ;; TODO: make nes/instruction-* take penalty-cycle parameter
       ;; TODO: make nes/instruction-* function return taken-cycles
       (if (or
            (equal "LDA" instruction-name)
            (equal "LDX" instruction-name)
            (equal "LDY" instruction-name)
            (equal "LAX" instruction-name)
            (equal "NOPI" instruction-name)
            (> (nes/cpu->cycles cpu) 0))
           penalty-cycles
         0))))

(defun nes/cpu-init (cpu)
  "Init CPU."
  (nes/cpu-reset cpu))

(provide 'nes-cpu)

;; Local Variables:
;; coding: utf-8
;; End:
;;; nes-cpu.el ends here
