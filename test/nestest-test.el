;;; nestest-test.el --- nestest.nes test -*- lexical-binding: t -*-

;; Author: Gabriele Lana <gabriele.lana@gmail.com>
;; Maintainer: Gabriele Lana <gabriele.lana@gmail.com>

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Run nestest.nes programmatically and check if everything is ok

;;; Code:

;;; TODO: why do we need it?
(require 'cl-lib)
(require 'f)
(require 'nes)
;; https://github.com/qpalzmqaz123/tsnes/blob/master/src/cpu/__tests__/nestest/instr.test.ts

(ert-deftest nestest-shall-pass ()
  ;; TODO: path to rom relative
  (let ((emulator (nes-setup "/home/coder/code/emacs-nes/test/roms/nestest.nes" nil)))
    ;; we are able to load a cartridge
    (should emulator)

    (let* ((c (nes-cpu emulator))
           (p (nes-ppu emulator))
           (r (nes/cpu->register c))
           (cyc 7)
           (log (f-read-text "/home/coder/code/emacs-nes/test/nestest.log"))
           (line-counter 0))
      ;; this.emulator.cpu.reset();
      ;; this.emulator.cpu.registers.PC = 0xC000;
      (setf (nes/cpu-register->pc r) #xc000)
      ;; this.emulator.cpu.setFlag(Flags.I, true);
      (setf (nes/cpu-register->sr-interrupt r) t)
      ;; this.emulator.cpu.setFlag(Flags.U, true);
      (setf (nes/cpu-register->sr-reserved r) t)
      (setf (nes/cpu-register->sr-break r) nil)

      ;; NOTE: we don't have it, it delays the cpu execution to synchronize it with the ppu
      ;; this.emulator.cpu.deferCycles = 7;
      ;; this.emulator.ppu.cycle = -21;

      ;; this.emulator.ppu.scanLine = 0;
      (setf (nes/ppu->line p) 0)

      (dolist (line (split-string log "\n" t))
        (cl-incf line-counter)
        (let* ((line-header (format "%05d: " line-counter))
               (expected (concat line-header (substring line 0 4) " " (substring line 48)))
               (given (concat line-header (nes/snapshot c p r cyc))))
          (should (equal expected given))
          (let ((cpu-cycles (nes/cpu-step-count c 1)))
            (cl-incf cyc cpu-cycles)
            (nes/ppu-step-count p (* cpu-cycles 3)))))
      (should (equal #x00 (nes/cpu-read c #x02)))
      (should (equal #x00 (nes/cpu-read c #x03))))))

(defun nes/snapshot (cpu ppu reg cyc)
  "Return a string/line representing the current hw state.

CPU is the CPU
PPU is the PPU
REG are the CPU registers
CYC global CPU clock counter."
  ;; const PC = this.emulator.cpu.registers.PC;
  ;; const A = this.emulator.cpu.registers.A;
  ;; const X = this.emulator.cpu.registers.X;
  ;; const Y = this.emulator.cpu.registers.Y;
  ;; const P = this.emulator.cpu.registers.P;
  ;; const SP = this.emulator.cpu.registers.SP;
  ;; const CYC = this.emulator.cpu.clocks;
  ;; const PPU = [this.emulator.ppu.cycle, this.emulator.ppu.scanLine];
  (let* ((pc (nes/cpu-register->pc reg))
         (a (nes/cpu-register->acc reg))
         (x (nes/cpu-register->idx-x reg))
         (y (nes/cpu-register->idx-y reg))
         (p (nes/cpu-status-register cpu))
         (sp (nes/cpu-register->sp reg))
         ;; TODO: use internal cycles to track global clock counter
         ;; (cyc (nes/cpu->cycles cpu))         ; ???
         (pcyc (nes/ppu->cycle ppu))
         (psl (nes/ppu->line ppu))
         ;; TODO: (nes/cpu-current-op c)
         ;; (op (nes/cpu--peek cpu))
         )
    ;; const log = sprintf('%04X A:%02X X:%02X Y:%02X P:%02X SP:%02X PPU:%3d,%3d CYC:%d',
    ;;                           PC, A, X, Y, P, SP, PPU[0], PPU[1], CYC);
    (format "%04X A:%02X X:%02X Y:%02X P:%02X SP:%02X PPU:%3d,%3d CYC:%d" pc a x y p sp pcyc psl cyc)))


(provide 'example-test)

;; Local Variables:
;; coding: utf-8
;; End:
;;; nestest-test.el ends here
