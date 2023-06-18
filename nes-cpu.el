;; -*- lexical-binding: t -*-

(eval-when-compile (require 'cl-lib))

(require 'nes-dma)
(require 'nes-interrupt)
(require 'nes-keypad)
(require 'nes-ppu)
(require 'nes-util)

(defconst nes/cpu--instructions (make-vector 256 nil))

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
  (ppu nil)
  (dma nil)
  (keypad nil)
  (bus (make-nes/cpu-bus))
  (register (make-nes/cpu-register))
  (interrupt nil))

(defun nes/cpu-init (cpu)
  "Initialize CPU."
  (nes/cpu-reset cpu)
  (aset nes/cpu--instructions #x00 '(:BRK :implied 7))
  (aset nes/cpu--instructions #x01 '(:ORA :pre-indexed-indirect 6))
  (aset nes/cpu--instructions #x02 '(:NOP :implied 2))
  (aset nes/cpu--instructions #x03 '(:SLO :pre-indexed-indirect 8))
  (aset nes/cpu--instructions #x04 '(:NOPD :implied 3))
  (aset nes/cpu--instructions #x05 '(:ORA :zero-page 3))
  (aset nes/cpu--instructions #x06 '(:ASL :zero-page 5))
  (aset nes/cpu--instructions #x07 '(:SLO :zero-page 5))
  (aset nes/cpu--instructions #x08 '(:PHP :implied 3))
  (aset nes/cpu--instructions #x09 '(:ORA :immediate 2))
  (aset nes/cpu--instructions #x0A '(:ASL :accumulator 2))
  (aset nes/cpu--instructions #x0B '(:NOP :implied 2))
  (aset nes/cpu--instructions #x0C '(:NOPI :absolute 4))
  (aset nes/cpu--instructions #x0D '(:ORA :absolute 4))
  (aset nes/cpu--instructions #x0E '(:ASL :absolute 6))
  (aset nes/cpu--instructions #x0F '(:SLO :absolute 6))
  (aset nes/cpu--instructions #x10 '(:BPL :relative 2))
  (aset nes/cpu--instructions #x11 '(:ORA :post-indexed-indirect 5))
  (aset nes/cpu--instructions #x12 '(:NOP :implied 2))
  (aset nes/cpu--instructions #x13 '(:SLO :post-indexed-indirect 8))
  (aset nes/cpu--instructions #x14 '(:NOPD :implied 4))
  (aset nes/cpu--instructions #x15 '(:ORA :zero-page-x 4))
  (aset nes/cpu--instructions #x16 '(:ASL :zero-page-x 6))
  (aset nes/cpu--instructions #x17 '(:SLO :zero-page-x 6))
  (aset nes/cpu--instructions #x18 '(:CLC :implied 2))
  (aset nes/cpu--instructions #x19 '(:ORA :absolute-y 4))
  (aset nes/cpu--instructions #x1A '(:NOP :implied 2))
  (aset nes/cpu--instructions #x1B '(:SLO :absolute-y 7))
  (aset nes/cpu--instructions #x1C '(:NOPI :absolute-x 4))
  (aset nes/cpu--instructions #x1D '(:ORA :absolute-x 4))
  (aset nes/cpu--instructions #x1E '(:ASL :absolute-x 7))
  (aset nes/cpu--instructions #x1F '(:SLO :absolute-x 7))
  (aset nes/cpu--instructions #x20 '(:JSR :absolute 6))
  (aset nes/cpu--instructions #x21 '(:AND :pre-indexed-indirect 6))
  (aset nes/cpu--instructions #x22 '(:NOP :implied 2))
  (aset nes/cpu--instructions #x23 '(:RLA :pre-indexed-indirect 8))
  (aset nes/cpu--instructions #x24 '(:BIT :zero-page 3))
  (aset nes/cpu--instructions #x25 '(:AND :zero-page 3))
  (aset nes/cpu--instructions #x26 '(:ROL :zero-page 5))
  (aset nes/cpu--instructions #x27 '(:RLA :zero-page 5))
  (aset nes/cpu--instructions #x28 '(:PLP :implied 4))
  (aset nes/cpu--instructions #x29 '(:AND :immediate 2))
  (aset nes/cpu--instructions #x2A '(:ROL :accumulator 2))
  (aset nes/cpu--instructions #x2B '(:NOP :implied 2))
  (aset nes/cpu--instructions #x2C '(:BIT :absolute 4))
  (aset nes/cpu--instructions #x2D '(:AND :absolute 4))
  (aset nes/cpu--instructions #x2E '(:ROL :absolute 6))
  (aset nes/cpu--instructions #x2F '(:RLA :absolute 6))
  (aset nes/cpu--instructions #x30 '(:BMI :relative 2))
  (aset nes/cpu--instructions #x31 '(:AND :post-indexed-indirect 5))
  (aset nes/cpu--instructions #x32 '(:NOP :implied 2))
  (aset nes/cpu--instructions #x33 '(:RLA :post-indexed-indirect 8))
  (aset nes/cpu--instructions #x34 '(:NOPD :implied 4))
  (aset nes/cpu--instructions #x35 '(:AND :zero-page-x 4))
  (aset nes/cpu--instructions #x36 '(:ROL :zero-page-x 6))
  (aset nes/cpu--instructions #x37 '(:RLA :zero-page-x 6))
  (aset nes/cpu--instructions #x38 '(:SEC :implied 2))
  (aset nes/cpu--instructions #x39 '(:AND :absolute-y 4))
  (aset nes/cpu--instructions #x3A '(:NOP :implied 2))
  (aset nes/cpu--instructions #x3B '(:RLA :absolute-y 7))
  (aset nes/cpu--instructions #x3C '(:NOPI :absolute-x 4))
  (aset nes/cpu--instructions #x3D '(:AND :absolute-x 4))
  (aset nes/cpu--instructions #x3E '(:ROL :absolute-x 7))
  (aset nes/cpu--instructions #x3F '(:RLA :absolute-x 7))
  (aset nes/cpu--instructions #x40 '(:RTI :implied 6))
  (aset nes/cpu--instructions #x41 '(:EOR :pre-indexed-indirect 6))
  (aset nes/cpu--instructions #x42 '(:NOP :implied 2))
  (aset nes/cpu--instructions #x43 '(:SRE :pre-indexed-indirect 8))
  (aset nes/cpu--instructions #x44 '(:NOPD :implied 3))
  (aset nes/cpu--instructions #x45 '(:EOR :zero-page 3))
  (aset nes/cpu--instructions #x46 '(:LSR :zero-page 5))
  (aset nes/cpu--instructions #x47 '(:SRE :zero-page 5))
  (aset nes/cpu--instructions #x48 '(:PHA :implied 3))
  (aset nes/cpu--instructions #x49 '(:EOR :immediate 2))
  (aset nes/cpu--instructions #x4A '(:LSR :accumulator 2))
  (aset nes/cpu--instructions #x4B '(:NOP :implied 2))
  (aset nes/cpu--instructions #x4C '(:JMP :absolute 3))
  (aset nes/cpu--instructions #x4D '(:EOR :absolute 4))
  (aset nes/cpu--instructions #x4E '(:LSR :absolute 6))
  (aset nes/cpu--instructions #x4F '(:SRE :absolute 6))
  (aset nes/cpu--instructions #x50 '(:BVC :relative 2))
  (aset nes/cpu--instructions #x51 '(:EOR :post-indexed-indirect 5))
  (aset nes/cpu--instructions #x52 '(:NOP :implied 2))
  (aset nes/cpu--instructions #x53 '(:SRE :post-indexed-indirect 8))
  (aset nes/cpu--instructions #x54 '(:NOPD :implied 4))
  (aset nes/cpu--instructions #x55 '(:EOR :zero-page-x 4))
  (aset nes/cpu--instructions #x56 '(:LSR :zero-page-x 6))
  (aset nes/cpu--instructions #x57 '(:SRE :zero-page-x 6))
  (aset nes/cpu--instructions #x58 '(:CLI :implied 2))
  (aset nes/cpu--instructions #x59 '(:EOR :absolute-y 4))
  (aset nes/cpu--instructions #x5A '(:NOP :implied 2))
  (aset nes/cpu--instructions #x5B '(:SRE :absolute-y 7))
  (aset nes/cpu--instructions #x5C '(:NOPI :absolute-x 4))
  (aset nes/cpu--instructions #x5D '(:EOR :absolute-x 4))
  (aset nes/cpu--instructions #x5E '(:LSR :absolute-x 7))
  (aset nes/cpu--instructions #x5F '(:SRE :absolute-x 7))
  (aset nes/cpu--instructions #x60 '(:RTS :implied 6))
  (aset nes/cpu--instructions #x61 '(:ADC :pre-indexed-indirect 6))
  (aset nes/cpu--instructions #x62 '(:NOP :implied 2))
  (aset nes/cpu--instructions #x63 '(:RRA :pre-indexed-indirect 8))
  (aset nes/cpu--instructions #x64 '(:NOPD :implied 3))
  (aset nes/cpu--instructions #x65 '(:ADC :zero-page 3))
  (aset nes/cpu--instructions #x66 '(:ROR :zero-page 5))
  (aset nes/cpu--instructions #x67 '(:RRA :zero-page 5))
  (aset nes/cpu--instructions #x68 '(:PLA :implied 4))
  (aset nes/cpu--instructions #x69 '(:ADC :immediate 2))
  (aset nes/cpu--instructions #x6A '(:ROR :accumulator 2))
  (aset nes/cpu--instructions #x6B '(:NOP :implied 2))
  (aset nes/cpu--instructions #x6C '(:JMP :indirect-absolute 5))
  (aset nes/cpu--instructions #x6D '(:ADC :absolute 4))
  (aset nes/cpu--instructions #x6E '(:ROR :absolute 6))
  (aset nes/cpu--instructions #x6F '(:RRA :absolute 6))
  (aset nes/cpu--instructions #x70 '(:BVS :relative 2))
  (aset nes/cpu--instructions #x71 '(:ADC :post-indexed-indirect 5))
  (aset nes/cpu--instructions #x72 '(:NOP :implied 2))
  (aset nes/cpu--instructions #x73 '(:RRA :post-indexed-indirect 8))
  (aset nes/cpu--instructions #x74 '(:NOPD :implied 4))
  (aset nes/cpu--instructions #x75 '(:ADC :zero-page-x 4))
  (aset nes/cpu--instructions #x76 '(:ROR :zero-page-x 6))
  (aset nes/cpu--instructions #x77 '(:RRA :zero-page-x 6))
  (aset nes/cpu--instructions #x78 '(:SEI :implied 2))
  (aset nes/cpu--instructions #x79 '(:ADC :absolute-y 4))
  (aset nes/cpu--instructions #x7A '(:NOP :implied 2))
  (aset nes/cpu--instructions #x7B '(:RRA :absolute-y 7))
  (aset nes/cpu--instructions #x7C '(:NOPI :absolute-x 4))
  (aset nes/cpu--instructions #x7D '(:ADC :absolute-x 4))
  (aset nes/cpu--instructions #x7E '(:ROR :absolute-x 7))
  (aset nes/cpu--instructions #x7F '(:RRA :absolute-x 7))
  (aset nes/cpu--instructions #x80 '(:NOPD :implied 2))
  (aset nes/cpu--instructions #x81 '(:STA :pre-indexed-indirect 6))
  (aset nes/cpu--instructions #x82 '(:NOPD :implied 2))
  (aset nes/cpu--instructions #x83 '(:SAX :pre-indexed-indirect 6))
  (aset nes/cpu--instructions #x84 '(:STY :zero-page 3))
  (aset nes/cpu--instructions #x85 '(:STA :zero-page 3))
  (aset nes/cpu--instructions #x86 '(:STX :zero-page 3))
  (aset nes/cpu--instructions #x87 '(:SAX :zero-page 3))
  (aset nes/cpu--instructions #x88 '(:DEY :implied 2))
  (aset nes/cpu--instructions #x89 '(:NOPD :implied 2))
  (aset nes/cpu--instructions #x8A '(:TXA :implied 2))
  (aset nes/cpu--instructions #x8B '(:NOP :implied 2))
  (aset nes/cpu--instructions #x8C '(:STY :absolute 4))
  (aset nes/cpu--instructions #x8D '(:STA :absolute 4))
  (aset nes/cpu--instructions #x8E '(:STX :absolute 4))
  (aset nes/cpu--instructions #x8F '(:SAX :absolute 4))
  (aset nes/cpu--instructions #x90 '(:BCC :relative 2))
  (aset nes/cpu--instructions #x91 '(:STA :post-indexed-indirect 6))
  (aset nes/cpu--instructions #x92 '(:NOP :implied 2))
  (aset nes/cpu--instructions #x93 '(:NOP :implied 6))
  (aset nes/cpu--instructions #x94 '(:STY :zero-page-x 4))
  (aset nes/cpu--instructions #x95 '(:STA :zero-page-x 4))
  (aset nes/cpu--instructions #x96 '(:STX :zero-page-y 4))
  (aset nes/cpu--instructions #x97 '(:SAX :zero-page-y 4))
  (aset nes/cpu--instructions #x98 '(:TYA :implied 2))
  (aset nes/cpu--instructions #x99 '(:STA :absolute-y 5))
  (aset nes/cpu--instructions #x9A '(:TXS :implied 2))
  (aset nes/cpu--instructions #x9B '(:NOP :implied 5))
  (aset nes/cpu--instructions #x9C '(:NOP :implied 5))
  (aset nes/cpu--instructions #x9D '(:STA :absolute-x 5))
  (aset nes/cpu--instructions #x9E '(:NOP :implied 5))
  (aset nes/cpu--instructions #x9F '(:NOP :implied 5))
  (aset nes/cpu--instructions #xA0 '(:LDY :immediate 2))
  (aset nes/cpu--instructions #xA1 '(:LDA :pre-indexed-indirect 6))
  (aset nes/cpu--instructions #xA2 '(:LDX :immediate 2))
  (aset nes/cpu--instructions #xA3 '(:LAX :pre-indexed-indirect 6))
  (aset nes/cpu--instructions #xA4 '(:LDY :zero-page 3))
  (aset nes/cpu--instructions #xA5 '(:LDA :zero-page 3))
  (aset nes/cpu--instructions #xA6 '(:LDX :zero-page 3))
  (aset nes/cpu--instructions #xA7 '(:LAX :zero-page 3))
  (aset nes/cpu--instructions #xA8 '(:TAY :implied 2))
  (aset nes/cpu--instructions #xA9 '(:LDA :immediate 2))
  (aset nes/cpu--instructions #xAA '(:TAX :implied 2))
  (aset nes/cpu--instructions #xAB '(:NOP :implied 2))
  (aset nes/cpu--instructions #xAC '(:LDY :absolute 4))
  (aset nes/cpu--instructions #xAD '(:LDA :absolute 4))
  (aset nes/cpu--instructions #xAE '(:LDX :absolute 4))
  (aset nes/cpu--instructions #xAF '(:LAX :absolute 4))
  (aset nes/cpu--instructions #xB0 '(:BCS :relative 2))
  (aset nes/cpu--instructions #xB1 '(:LDA :post-indexed-indirect 5))
  (aset nes/cpu--instructions #xB2 '(:NOP :implied 2))
  (aset nes/cpu--instructions #xB3 '(:LAX :post-indexed-indirect 5))
  (aset nes/cpu--instructions #xB4 '(:LDY :zero-page-x 4))
  (aset nes/cpu--instructions #xB5 '(:LDA :zero-page-x 4))
  (aset nes/cpu--instructions #xB6 '(:LDX :zero-page-y 4))
  (aset nes/cpu--instructions #xB7 '(:LAX :zero-page-y 4))
  (aset nes/cpu--instructions #xB8 '(:CLV :implied 2))
  (aset nes/cpu--instructions #xB9 '(:LDA :absolute-y 4))
  (aset nes/cpu--instructions #xBA '(:TSX :implied 2))
  (aset nes/cpu--instructions #xBB '(:NOP :implied 4))
  (aset nes/cpu--instructions #xBC '(:LDY :absolute-x 4))
  (aset nes/cpu--instructions #xBD '(:LDA :absolute-x 4))
  (aset nes/cpu--instructions #xBE '(:LDX :absolute-y 4))
  (aset nes/cpu--instructions #xBF '(:LAX :absolute-y 4))
  (aset nes/cpu--instructions #xC0 '(:CPY :immediate 2))
  (aset nes/cpu--instructions #xC1 '(:CMP :pre-indexed-indirect 6))
  (aset nes/cpu--instructions #xC2 '(:NOPD :implied 2))
  (aset nes/cpu--instructions #xC3 '(:DCP :pre-indexed-indirect 8))
  (aset nes/cpu--instructions #xC4 '(:CPY :zero-page 3))
  (aset nes/cpu--instructions #xC5 '(:CMP :zero-page 3))
  (aset nes/cpu--instructions #xC6 '(:DEC :zero-page 5))
  (aset nes/cpu--instructions #xC7 '(:DCP :zero-page 5))
  (aset nes/cpu--instructions #xC8 '(:INY :implied 2))
  (aset nes/cpu--instructions #xC9 '(:CMP :immediate 2))
  (aset nes/cpu--instructions #xCA '(:DEX :implied 2))
  (aset nes/cpu--instructions #xCB '(:NOP :implied 2))
  (aset nes/cpu--instructions #xCC '(:CPY :absolute 4))
  (aset nes/cpu--instructions #xCD '(:CMP :absolute 4))
  (aset nes/cpu--instructions #xCE '(:DEC :absolute 6))
  (aset nes/cpu--instructions #xCF '(:DCP :absolute 6))
  (aset nes/cpu--instructions #xD0 '(:BNE :relative 2))
  (aset nes/cpu--instructions #xD1 '(:CMP :post-indexed-indirect 5))
  (aset nes/cpu--instructions #xD2 '(:NOP :implied 2))
  (aset nes/cpu--instructions #xD3 '(:DCP :post-indexed-indirect 8))
  (aset nes/cpu--instructions #xD4 '(:NOPD :implied 4))
  (aset nes/cpu--instructions #xD5 '(:CMP :zero-page-x 4))
  (aset nes/cpu--instructions #xD6 '(:DEC :zero-page-x 6))
  (aset nes/cpu--instructions #xD7 '(:DCP :zero-page-x 6))
  (aset nes/cpu--instructions #xD8 '(:CLD :implied 2))
  (aset nes/cpu--instructions #xD9 '(:CMP :absolute-y 4))
  (aset nes/cpu--instructions #xDA '(:NOP :implied 2))
  (aset nes/cpu--instructions #xDB '(:DCP :absolute-y 7))
  (aset nes/cpu--instructions #xDC '(:NOPI :absolute-x 4))
  (aset nes/cpu--instructions #xDD '(:CMP :absolute-x 4))
  (aset nes/cpu--instructions #xDE '(:DEC :absolute-x 7))
  (aset nes/cpu--instructions #xDF '(:DCP :absolute-x 7))
  (aset nes/cpu--instructions #xE0 '(:CPX :immediate 2))
  (aset nes/cpu--instructions #xE1 '(:SBC :pre-indexed-indirect 6))
  (aset nes/cpu--instructions #xE2 '(:NOPD :implied 3))
  (aset nes/cpu--instructions #xE3 '(:ISB :pre-indexed-indirect 8))
  (aset nes/cpu--instructions #xE4 '(:CPX :zero-page 3))
  (aset nes/cpu--instructions #xE5 '(:SBC :zero-page 3))
  (aset nes/cpu--instructions #xE6 '(:INC :zero-page 5))
  (aset nes/cpu--instructions #xE7 '(:ISB :zero-page 5))
  (aset nes/cpu--instructions #xE8 '(:INX :implied 2))
  (aset nes/cpu--instructions #xE9 '(:SBC :immediate 2))
  (aset nes/cpu--instructions #xEA '(:NOP :implied 2))
  (aset nes/cpu--instructions #xEB '(:SBC :immediate 2))
  (aset nes/cpu--instructions #xEC '(:CPX :absolute 4))
  (aset nes/cpu--instructions #xED '(:SBC :absolute 4))
  (aset nes/cpu--instructions #xEE '(:INC :absolute 6))
  (aset nes/cpu--instructions #xEF '(:ISB :absolute 6))
  (aset nes/cpu--instructions #xF0 '(:BEQ :relative 2))
  (aset nes/cpu--instructions #xF1 '(:SBC :post-indexed-indirect 5))
  (aset nes/cpu--instructions #xF2 '(:NOP :implied 2))
  (aset nes/cpu--instructions #xF3 '(:ISB :post-indexed-indirect 8))
  (aset nes/cpu--instructions #xF4 '(:NOPD :implied 4))
  (aset nes/cpu--instructions #xF5 '(:SBC :zero-page-x 4))
  (aset nes/cpu--instructions #xF6 '(:INC :zero-page-x 6))
  (aset nes/cpu--instructions #xF7 '(:ISB :zero-page-x 6))
  (aset nes/cpu--instructions #xF8 '(:SED :implied 2))
  (aset nes/cpu--instructions #xF9 '(:SBC :absolute-y 4))
  (aset nes/cpu--instructions #xFA '(:NOP :implied 2))
  (aset nes/cpu--instructions #xFB '(:ISB :absolute-y 7))
  (aset nes/cpu--instructions #xFC '(:NOPI :absolute-x 4))
  (aset nes/cpu--instructions #xFD '(:SBC :absolute-x 4))
  (aset nes/cpu--instructions #xFE '(:INC :absolute-x 7))
  (aset nes/cpu--instructions #xFF '(:ISB :absolute-x 7)))

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
  (let ((d (nes/cpu-pull cpu))
        (r (nes/cpu->register cpu)))
    (setf (nes/cpu-register->sr-negative r)  (nes--logbitp 7 d)
          (nes/cpu-register->sr-overflow r)  (nes--logbitp 6 d)
          (nes/cpu-register->sr-reserved r)  (nes--logbitp 5 d)
          (nes/cpu-register->sr-break r)     (nes--logbitp 4 d)
          (nes/cpu-register->sr-decimal r)   (nes--logbitp 3 d)
          (nes/cpu-register->sr-interrupt r) (nes--logbitp 2 d)
          (nes/cpu-register->sr-zero r)      (nes--logbitp 1 d)
          (nes/cpu-register->sr-carry r)     (nes--logbitp 0 d))))

(defun nes/cpu-reset (cpu)
  "Reset CPU."
  (let ((r (nes/cpu->register cpu)))
    (setf (nes/cpu-register->sp r) #xFD
          (nes/cpu-register->pc r) (nes/cpu-read cpu #xFFFC :word)
          (nes/cpu-register->sr-carry r) nil
          (nes/cpu-register->sr-zero r) nil
          (nes/cpu-register->sr-interrupt r) t
          (nes/cpu-register->sr-decimal r) nil
          (nes/cpu-register->sr-break r) t
          (nes/cpu-register->sr-reserved r) t
          (nes/cpu-register->sr-overflow r) nil
          (nes/cpu-register->sr-negative r) nil)))

(defun nes/cpu-nmi (c)
  (let ((register (nes/cpu->register c)))
    (nes/interrupt-deassert-nmi (nes/cpu->interrupt c))
    (setf (nes/cpu-register->sr-break register) nil)
    (nes/cpu-push c (logand #xFF (ash (nes/cpu-register->pc register) -8)))
    (nes/cpu-push c (logand #xFF (nes/cpu-register->pc register)))
    (nes/cpu-push-status-register c)
    (setf (nes/cpu-register->sr-interrupt register) t
          (nes/cpu-register->pc register) (nes/cpu-read c #xFFFA :word))))

(defun nes/cpu-irq (c)
  (let ((register (nes/cpu->register c)))
    (nes/cpu-push c (logand #xff (ash (nes/cpu-register->pc register) -8)))
    (nes/cpu-push c (logand #xff (nes/cpu-register->pc register)))
    (nes/cpu-push-status-register c)
    (setf (nes/cpu-register->sr-interrupt register) t
          (nes/cpu-register->pc register) (nes/cpu-read c #xfffe :word))))

(defun nes/cpu--get-instruction-operand-and-cycle (cpu mode)
  "Return current operand and cycle's cost given an addressing MODE of CPU."
  (let ((register (nes/cpu->register cpu))
        base-addr
        data-addr
        addr
        idx-x
        idx-y
        cycle)
    ;; Sorted, most used addressing mode first, according to
    ;; http://web.archive.org/web/20190319195151/http://blargg.8bitalley.com/nes-emu/6502.html
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

(defmacro nes/instruction--set-zero-and-negative-flags (r data)
  "TODO R DATA."
  `(progn
     (setf (nes/cpu-register->sr-zero ,r) (zerop (logand ,data #xFF)))
     (setf (nes/cpu-register->sr-negative ,r) (/= (logand ,data (ash 1 7)) 0)
           ;; (nes--logbitp 7 ,data)
           )))

(defun nes/cpu-step-count (cpu count)
  "Execute next COUNT CPU instructions."
  (let ((total-cycles 0)
        opcode
        r0
        sym
        mode
        cost
        r1
        op
        penalty
        register
        data)
    (dotimes (_ count)
      (when (nes/interrupt->nmi (nes/cpu->interrupt cpu))
        (nes/cpu-nmi cpu))
      (nes/interrupt-clear (nes/cpu->interrupt cpu))
      (setq opcode (nes/cpu--fetch cpu)
            r0 (aref nes/cpu--instructions opcode)
            sym (nth 0 r0)
            mode (nth 1 r0)
            cost (nth 2 r0)
            r1 (nes/cpu--get-instruction-operand-and-cycle cpu mode)
            op (car r1)
            penalty (cdr r1)
            register (nes/cpu->register cpu)
            total-cycles (+ total-cycles cost)
            data nil)
      (cond
       ;; LDA: Load Accumulator
       ((eq sym :LDA)
        (setq data (if (eq mode :immediate) op (nes/cpu-read cpu op))
              total-cycles (+ total-cycles penalty))
        (setf (nes/cpu-register->acc register) data)
        (nes/instruction--set-zero-and-negative-flags register data))
       ;; BNE
       ((eq sym :BNE)
        (unless (nes/cpu-register->sr-zero register)
          (setq total-cycles (+ total-cycles 1 penalty))
          (setf (nes/cpu-register->pc register) op)))
       ;; JMP: Jump
       ((eq sym :JMP)
        (setf (nes/cpu-register->pc (nes/cpu->register cpu)) op))
       ;; INX: Increment X Register
       ((eq sym :INX)
        (setq data (logand (1+ (nes/cpu-register->idx-x register)) #xff))
        (setf (nes/cpu-register->idx-x register) data)
        (nes/instruction--set-zero-and-negative-flags register data))
       ;; BPL
       ((eq sym :BPL)
        (unless (nes/cpu-register->sr-negative register)
          (setq total-cycles (+ total-cycles 1 penalty))
          (setf (nes/cpu-register->pc register) op)))
       ;; CMP: Compare
       ((eq sym :CMP)
        (setq data (if (eq mode :immediate) op (nes/cpu-read cpu op)))
        (let ((compared (- (nes/cpu-register->acc register) data)))
          (nes/instruction--set-zero-and-negative-flags register compared)
          (setf (nes/cpu-register->sr-carry register) (>= compared 0))))
       ;; BMI
       ((eq sym :BMI)
        (when (nes/cpu-register->sr-negative register)
          (setq total-cycles (+ total-cycles 1 penalty))
          (setf (nes/cpu-register->pc register) op)))
       ;; BEQ
       ((eq sym :BEQ)
        (when (nes/cpu-register->sr-zero register)
          (setq total-cycles (+ total-cycles 1 penalty))
          (setf (nes/cpu-register->pc register) op)))
       ;; BIT
       ((eq sym :BIT)
        (setq data (nes/cpu-read cpu op))
        (setf (nes/cpu-register->sr-negative register) (nes--logbitp 7 data)
              (nes/cpu-register->sr-overflow register) (nes--logbitp 6 data)
              (nes/cpu-register->sr-zero register) (zerop (logand (nes/cpu-register->acc register) data))))
       ;; STA: Store Accumulator
       ((eq sym :STA)
        (nes/cpu-write cpu op (nes/cpu-register->acc (nes/cpu->register cpu))))
       ;; DEX: Decrement X Register
       ((eq sym :DEX)
        (setq data (logand (1- (nes/cpu-register->idx-x register)) #xff))
        (setf (nes/cpu-register->idx-x register) data)
        (nes/instruction--set-zero-and-negative-flags register data))
       ;; INY: Increment Y Register
       ((eq sym :INY)
        (setq data (logand (1+ (nes/cpu-register->idx-y register)) #xff))
        (setf (nes/cpu-register->idx-y register) data)
        (nes/instruction--set-zero-and-negative-flags register data))
       ;; TAY: Transfer Accumulator to Y
       ((eq sym :TAY)
        (setq data (nes/cpu-register->acc register))
        (setf (nes/cpu-register->idx-y register) data)
        (nes/instruction--set-zero-and-negative-flags register data))
       ;; LDX: Load X Register
       ((eq sym :LDX)
        (setq data (if (eq mode :immediate) op (nes/cpu-read cpu op))
              total-cycles (+ total-cycles penalty))
        (setf (nes/cpu-register->idx-x register) data)
        (nes/instruction--set-zero-and-negative-flags register data))
       ;; LDY: Load Y Register
       ((eq sym :LDY)
        (setq data (if (eq mode :immediate) op (nes/cpu-read cpu op))
              total-cycles (+ total-cycles penalty))
        (setf (nes/cpu-register->idx-y register) data)
        (nes/instruction--set-zero-and-negative-flags register data))
       ;; STX: Store X Register
       ((eq sym :STX)
        (nes/cpu-write cpu op (nes/cpu-register->idx-x (nes/cpu->register cpu))))
       ;; STY: Store Y Register
       ((eq sym :STY)
        (nes/cpu-write cpu op (nes/cpu-register->idx-y (nes/cpu->register cpu))))
       ;; TAX: Transfer Accumulator to X
       ((eq sym :TAX)
        (setq data (nes/cpu-register->acc register))
        (setf (nes/cpu-register->idx-x register) data)
        (nes/instruction--set-zero-and-negative-flags register data))
       ;; TSX: Transfer Stack Pointer to X
       ((eq sym :TSX)
        (setq data (nes/cpu-register->sp register))
        (setf (nes/cpu-register->idx-x register) data)
        (nes/instruction--set-zero-and-negative-flags register data))
       ;; TXA: Transfer X to Accumulator
       ((eq sym :TXA)
        (setq data (nes/cpu-register->idx-x register))
        (setf (nes/cpu-register->acc register) data)
        (nes/instruction--set-zero-and-negative-flags register data))
       ;; TXS: Transfer X to Stack Pointer
       ((eq sym :TXS)
        (setf (nes/cpu-register->sp register)
              (nes/cpu-register->idx-x register)))
       ;; TYA: Transfer Y to Accumulator
       ((eq sym :TYA)
        (setq data (nes/cpu-register->idx-y register))
        (setf (nes/cpu-register->acc register) data)
        (nes/instruction--set-zero-and-negative-flags register data))
       ;; PHP: Push Processor Status
       ;; In the byte pushed, bit 5 is always set to 1, and bit 4 is 1 if from an instruction (PHP or BRK)
       ;; see https://wiki.nesdev.com/w/index.php/Status_flags#The_B_flag
       ((eq sym :PHP)
        (let ((old-break (nes/cpu-register->sr-break register))
              (old-reserved (nes/cpu-register->sr-reserved register)))
          (setf (nes/cpu-register->sr-break register) t
                (nes/cpu-register->sr-reserved register) t)
          (nes/cpu-push-status-register cpu)
          (setf (nes/cpu-register->sr-break register) old-break
                (nes/cpu-register->sr-reserved register) old-reserved)))
       ;; PHA: Push Accumulator
       ((eq sym :PHA)
        (nes/cpu-push cpu (nes/cpu-register->acc (nes/cpu->register cpu))))
       ;; PLA: Pull Accumulator
       ((eq sym :PLA)
        (setq data (nes/cpu-pull cpu))
        (setf (nes/cpu-register->acc register) data)
        (nes/instruction--set-zero-and-negative-flags register data))
       ;; PLP: Pull Processor Status
       ;; Two instructions (PLP and RTI) pull a byte from the stack and set all the flags. They ignore bits 5 and 4.
       ;; see https://wiki.nesdev.com/w/index.php/Status_flags#The_B_flag
       ((eq sym :PLP)
        (let ((current-break (nes/cpu-register->sr-break register))
              (current-reserved (nes/cpu-register->sr-reserved register)))
          (nes/cpu-pull-status-register cpu)
          (setf (nes/cpu-register->sr-break register) current-break
                (nes/cpu-register->sr-reserved register) current-reserved)))
       ;; SEC: Set Carry Flag
       ((eq sym :SEC)
        (setf (nes/cpu-register->sr-carry (nes/cpu->register cpu)) t))
       ;; SED: Set Decimal Flag
       ((eq sym :SED)
        (setf (nes/cpu-register->sr-decimal (nes/cpu->register cpu)) t))
       ;; SEI: Set Interrupt Disable
       ((eq sym :SEI)
        (setf (nes/cpu-register->sr-interrupt (nes/cpu->register cpu)) t))
       ;; CLC: Clear Carry Flag
       ((eq sym :CLC)
        (setf (nes/cpu-register->sr-carry (nes/cpu->register cpu)) nil))
       ;; CLD: Clear Decimal Mode
       ((eq sym :CLD)
        (setf (nes/cpu-register->sr-decimal (nes/cpu->register cpu)) nil))
       ;; CLI: Clear Interrupt Disable
       ((eq sym :CLI)
        (setf (nes/cpu-register->sr-interrupt (nes/cpu->register cpu)) nil))
       ;; CLV: Clear Overflow Flag
       ((eq sym :CLV)
        (setf (nes/cpu-register->sr-overflow (nes/cpu->register cpu)) nil))
       ;; INC: Increment Memory
       ((eq sym :INC)
        (setq data (logand (1+ (nes/cpu-read cpu op)) #xff))
        (nes/cpu-write cpu op data)
        (nes/instruction--set-zero-and-negative-flags register data))
       ;; DEC: Decrement Memory
       ((eq sym :DEC)
        (setq data (logand (1- (nes/cpu-read cpu op)) #xff))
        (nes/cpu-write cpu op data)
        (nes/instruction--set-zero-and-negative-flags register data))
       ;; DEY: Decrement Y Register
       ((eq sym :DEY)
        (setq data (logand (1- (nes/cpu-register->idx-y register)) #xff))
        (setf (nes/cpu-register->idx-y register) data)
        (nes/instruction--set-zero-and-negative-flags register data))
       ;; ADC: Add with Carry
       ((eq sym :ADC)
        (setq data (if (eq mode :immediate) op (nes/cpu-read cpu op)))
        (let* ((acc (nes/cpu-register->acc register))
               (result (+ acc
                          data
                          (if (nes/cpu-register->sr-carry register)
                              1 0))))
          (nes/instruction--set-zero-and-negative-flags register result)
          (setf (nes/cpu-register->sr-carry register) (> result #xff)
                (nes/cpu-register->sr-overflow register) (and (not (nes--logbitp 7 (logxor acc data)))
                                                              (nes--logbitp 7 (logxor acc result)))
                (nes/cpu-register->acc register) (logand result #xFF))))
       ;; SBC: Subtract with Carry
       ((eq sym :SBC)
        (setq data (if (eq mode :immediate) op (nes/cpu-read cpu op)))
        (let* ((acc (nes/cpu-register->acc register))
               (result (- acc
                          data
                          (if (nes/cpu-register->sr-carry register)
                              0 1))))
          (nes/instruction--set-zero-and-negative-flags register result)
          (setf (nes/cpu-register->sr-carry register) (>= result 0)
                (nes/cpu-register->sr-overflow register) (and (nes--logbitp 7 (logxor acc data))
                                                              (nes--logbitp 7 (logxor acc result)))
                (nes/cpu-register->acc register) (logand result #xFF))))
       ;; CPX: Compare X Register
       ((eq sym :CPX)
        (setq data (if (eq mode :immediate) op (nes/cpu-read cpu op)))
        (let ((compared (- (nes/cpu-register->idx-x register) data)))
          (nes/instruction--set-zero-and-negative-flags register compared)
          (setf (nes/cpu-register->sr-carry register) (>= compared 0))))
       ;; CPY: Compare Y Register
       ((eq sym :CPY)
        (setq data (if (eq mode :immediate) op (nes/cpu-read cpu op)))
        (let ((compared (- (nes/cpu-register->idx-y register) data)))
          (nes/instruction--set-zero-and-negative-flags register compared)
          (setf (nes/cpu-register->sr-carry register) (>= compared 0))))
       ;; AND: Logical And
       ((eq sym :AND)
        (setq data (if (eq mode :immediate) op (nes/cpu-read cpu op)))
        (let ((result (logand (nes/cpu-register->acc register) data)))
          (nes/instruction--set-zero-and-negative-flags register result)
          (setf (nes/cpu-register->acc register) (logand result #xff))))
       ;; ORA: Logical Inclusive OR
       ((eq sym :ORA)
        (setq data (if (eq mode :immediate) op (nes/cpu-read cpu op)))
        (let ((result (logior (nes/cpu-register->acc register) data)))
          (nes/instruction--set-zero-and-negative-flags register result)
          (setf (nes/cpu-register->acc register) (logand result #xff))))
       ;; EOR: Exclusive OR
       ((eq sym :EOR)
        (setq data (if (eq mode :immediate) op (nes/cpu-read cpu op)))
        (let ((result (logxor (nes/cpu-register->acc register) data)))
          (nes/instruction--set-zero-and-negative-flags register result)
          (setf (nes/cpu-register->acc register) (logand result #xff))))
       ;; ASL: Arithmetic Shift Left
       ((eq sym :ASL)
        (setq data (if (eq mode :accumulator) (nes/cpu-register->acc register) (nes/cpu-read cpu op)))
        (let ((shifted (logand (ash data 1) #xff)))
          (setf (nes/cpu-register->sr-carry register) (nes--logbitp 7 data))
          (nes/instruction--set-zero-and-negative-flags register shifted)
          (if (eq mode :accumulator)
              (setf (nes/cpu-register->acc register) shifted)
            (nes/cpu-write cpu op shifted))))
       ;; LSR: Logical Shift Right
       ((eq sym :LSR)
        (setq data (logand (if (eq mode :accumulator) (nes/cpu-register->acc register) (nes/cpu-read cpu op))))
        (let ((shifted (logand (ash data -1) #xff)))
          (setf (nes/cpu-register->sr-carry register) (nes--logbitp 0 data))
          (nes/instruction--set-zero-and-negative-flags register shifted)
          (if (eq mode :accumulator)
              (setf (nes/cpu-register->acc register) shifted)
            (nes/cpu-write cpu op shifted))))
       ;; ROL: Rotate Left
       ((eq sym :ROL)
        (setq data (logand (if (eq mode :accumulator) (nes/cpu-register->acc register) (nes/cpu-read cpu op))))
        (let* ((carry (nes/cpu-register->sr-carry register))
               (rotated (logand #xff
                                (logior (ash data 1) (if carry #x01 #x00)))))
          (setf (nes/cpu-register->sr-carry register) (nes--logbitp 7 data))
          (nes/instruction--set-zero-and-negative-flags register rotated)
          (if (eq mode :accumulator)
              (setf (nes/cpu-register->acc register) rotated)
            (nes/cpu-write cpu op rotated))))
       ;; ROR: Rotate Right
       ((eq sym :ROR)
        (setq data (logand (if (eq mode :accumulator) (nes/cpu-register->acc register) (nes/cpu-read cpu op))))
        (let* ((carry (nes/cpu-register->sr-carry register))
               (rotated (logand #xff
                                (logior (ash data -1) (if carry #x80 #x00)))))
          (setf (nes/cpu-register->sr-carry register) (/= (logand data #x01) 0))
          (nes/instruction--set-zero-and-negative-flags register rotated)
          (if (eq mode :accumulator)
              (setf (nes/cpu-register->acc register) rotated)
            (nes/cpu-write cpu op rotated))))
       ;; NOP: No Operation
       ((eq sym :NOP)
        ;; doesn nothing?
        '())
       ;; BVS
       ((eq sym :BVS)
        (when (nes/cpu-register->sr-overflow register)
          (setq total-cycles (+ total-cycles 1 penalty))
          (setf (nes/cpu-register->pc register) op)))
       ;; BVC
       ((eq sym :BVC)
        (unless (nes/cpu-register->sr-overflow register)
          (setq total-cycles (+ total-cycles 1 penalty))
          (setf (nes/cpu-register->pc register) op)))
       ;; BCS
       ((eq sym :BCS)
        (when (nes/cpu-register->sr-carry register)
          (setq total-cycles (+ total-cycles 1 penalty))
          (setf (nes/cpu-register->pc register) op)))
       ;; BCC
       ((eq sym :BCC)
        (unless (nes/cpu-register->sr-carry register)
          (setq total-cycles (+ total-cycles 1 penalty))
          (setf (nes/cpu-register->pc register) op)))
       ;; BRK
       ((eq sym :BRK)
        (let ((interrupt (nes/cpu-register->sr-interrupt register)))
          (setf (nes/cpu-register->sr-break register) t)
          (cl-incf (nes/cpu-register->pc register))
          (nes/cpu-push cpu (logand #xFF (ash (nes/cpu-register->pc register) -8)))
          (nes/cpu-push cpu (logand #xFF (nes/cpu-register->pc register)))
          (nes/cpu-push-status-register cpu)
          (unless interrupt
            (setf (nes/cpu-register->sr-interrupt register) t
                  (nes/cpu-register->pc register) (nes/cpu-read cpu #xFFFE :word)))
          (cl-decf (nes/cpu-register->pc register))))
       ;; JSR
       ((eq sym :JSR)
        (let ((pc (1- (nes/cpu-register->pc register))))
          (nes/cpu-push cpu (logand (ash pc -8) #xFF))
          (nes/cpu-push cpu (logand pc #xFF))
          (setf (nes/cpu-register->pc register) op)))
       ;; RTS
       ((eq sym :RTS)
        (setf (nes/cpu-register->pc register) (1+ (logior (nes/cpu-pull cpu)
                                                          (ash (nes/cpu-pull cpu) 8)))))
       ;; RTI
       ((eq sym :RTI)
        (nes/cpu-pull-status-register cpu)
        (setf (nes/cpu-register->sr-reserved register) t
              (nes/cpu-register->pc register)
              (logior (nes/cpu-pull cpu)
                      (ash (nes/cpu-pull cpu) 8))))
       ;; NOPD
       ((eq sym :NOPD)
        (setf (nes/cpu-register->pc register) (1+ (nes/cpu-register->pc register))))
       ;; NOPI
       ((eq sym :NOPI)
        (setq total-cycles (+ total-cycles penalty)))
       ;; LAX
       ((eq sym :LAX)
        (setq data (nes/cpu-read cpu op)
              total-cycles (+ total-cycles penalty))
        (setf (nes/cpu-register->idx-x register) data
              (nes/cpu-register->acc register) data)
        (nes/instruction--set-zero-and-negative-flags register data))
       ;; SAX
       ((eq sym :SAX)
        (nes/cpu-write cpu op
                       (logand (nes/cpu-register->acc register)
                               (nes/cpu-register->idx-x register))))
       ;; DCP
       ((eq sym :DCP)
        (let ((operated (logand (1- (nes/cpu-read cpu op)) #xFF)))
          (setf (nes/cpu-register->sr-negative register)
                (/= (logand (logand (- (nes/cpu-register->acc register) operated) #x1FF) #x80) 0)
                (nes/cpu-register->sr-zero register)
                (= (logand (- (nes/cpu-register->acc register) operated) #x1FF) 0))
          (nes/cpu-write cpu op operated)))
       ;; ISB
       ((eq sym :ISB)
        (setq data (logand (1+ (nes/cpu-read cpu op)) #xFF))
        (let ((operated (+ (logand (lognot data) #xFF)
                           (nes/cpu-register->acc register)
                           (if (nes/cpu-register->sr-carry register) 1 0))))
          (setf (nes/cpu-register->sr-overflow register)
                (and (zerop (logand (logxor (nes/cpu-register->acc register) data) #x80))
                     (not (zerop (logand (logxor (nes/cpu-register->acc register) operated) #x80))))
                (nes/cpu-register->sr-carry register)
                (> operated #xFF))
          (nes/instruction--set-zero-and-negative-flags register operated)
          (setf (nes/cpu-register->acc register) (logand operated #xFF))
          (nes/cpu-write cpu op data)))
       ;; SLO
       ((eq sym :SLO)
        (setq data (nes/cpu-read cpu op))
        (setf (nes/cpu-register->sr-carry register) (nes--logbitp 7 data))
        (setq data (logand (ash data 1) #xFF))
        (setf (nes/cpu-register->acc register) (logior (nes/cpu-register->acc register) data)
              (nes/cpu-register->sr-negative register) (nes--logbitp 7 (nes/cpu-register->acc register))
              (nes/cpu-register->sr-zero register) (= (logand (nes/cpu-register->acc register) #xFF) 0))
        (nes/cpu-write cpu op data))
       ;; RLA
       ((eq sym :RLA)
        (setq data (+ (ash (nes/cpu-read cpu op) 1)
                      (if (nes/cpu-register->sr-carry register) 1 0)))
        (setf (nes/cpu-register->sr-carry register) (nes--logbitp 8 data)
              (nes/cpu-register->acc register) (logand (logand (nes/cpu-register->acc register) data) #xFF)
              (nes/cpu-register->sr-negative register) (nes--logbitp 7 (nes/cpu-register->acc register))
              (nes/cpu-register->sr-zero register) (= (logand (nes/cpu-register->acc register) #xFF) 0))
        (nes/cpu-write cpu op data))
       ;; SRE
       ((eq sym :SRE)
        (setq data (nes/cpu-read cpu op))
        (setf (nes/cpu-register->sr-carry register) (nes--logbitp 0 data))
        (setq data (ash data -1))
        (setf (nes/cpu-register->acc register) (logxor (nes/cpu-register->acc register) data)
              (nes/cpu-register->sr-negative register) (nes--logbitp 7 (nes/cpu-register->acc register))
              (nes/cpu-register->sr-zero register) (= (logand (nes/cpu-register->acc register) #xFF) 0))
        (nes/cpu-write cpu op data))
       ;; RRA
       ((eq sym :RRA)
        (setq data (nes/cpu-read cpu op))
        (let ((carry (nes--logbitp 0 data))
              operated)
          (setq data (logior (ash data -1)
                             (if (nes/cpu-register->sr-carry register) #x80 #x00))
                operated (+ data (nes/cpu-register->acc register) (if carry 1 0)))
          (setf (nes/cpu-register->sr-overflow register) (and (not (nes--logbitp 7 (logxor (nes/cpu-register->acc register) data)))
                                                              (nes--logbitp 7 (logxor (nes/cpu-register->acc register) operated)))
                (nes/cpu-register->sr-carry register) (> operated #xFF)
                (nes/cpu-register->sr-negative register) (nes--logbitp 7 operated)
                (nes/cpu-register->sr-zero register) (= (logand operated #xFF) 0)
                (nes/cpu-register->acc register) (logand operated #xFF))
          (nes/cpu-write cpu op data)))
       (t (user-error "Unable to handle unknown opcode #x%02X" opcode))))
    total-cycles))

(provide 'nes-cpu)

;; Local Variables:
;; coding: utf-8
;; End:
;;; nes-cpu.el ends here
