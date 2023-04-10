;;; bench.el --- summary -*- lexical-binding: t -*-

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

;; summary

;;; Code:

(require 'f)
(require 'cl-lib)
(require 'nes)

(defconst NES/CPU-CLOCK
  1789773
  "On real CPU: number of CPU clock per second.")

(defconst NES/CPU-US-PER-OP
  (/ 1.0 (/ NES/CPU-CLOCK 3.5 1000000.0))
  "On real CPU: microseconds per OP (assuming ~3.5 clock per OP).")

;;; TODO: save somewhere the best result based on the name of the rom
(defconst NES/EMULATOR-US-PER-OP
  6.83
  "Emulator: microseconds per OP (best recorded so far).")

(defmacro bench-rom (repeat steps rom-path &optional setup)
  "Bench ROM with `benchmark-run-compiled'.

The benchmark is repeated a REPEAT number of times. The rom is
loaded at ROM-PATH and for each iteration will execute STEPS
number of steps.

You can setup the emulator before running the rom with SETUP
function, this function will take three arguments: CPU, REGISTERS
and PPU.

Return a list comparing the number of microseconds taken to run a
single step between: the current execution, the best recorded
execution and the real hardware execution.

The cost of loading the rom is taken in consideration."
  (setq setup (or setup '(lambda (_c _r _p) nil)))
  `(let* (us-per-op
          (repeat ,repeat)
          (steps ,steps)
          (takes-to-load (benchmark-run-compiled repeat
                           (nes-setup ,rom-path nil)))
          (takes-to-run (benchmark-run-compiled repeat
                          (let* ((emulator (nes-setup ,rom-path nil))
                                 (c (nes-cpu emulator))
                                 (r (nes/cpu->register c))
                                 (p (nes-ppu emulator)))
                            (,setup c r p)
                            (dotimes (_ ,steps)
                              (let ((cpu-cycles (nes/cpu-step c)))
                                (nes/ppu-step-count p (* cpu-cycles))))))))
     (setq us-per-op (* (/ (- (nth 0 takes-to-run) (nth 0 takes-to-load)) repeat steps) 1000000))
     (message (concat "CPU current emulation speed: %f microseconds per op\n"
                      "CPU best emulation speed: %f microseconds per op (%f)\n"
                      "CPU target speed: %f microseconds per op (%f)\n")
              us-per-op
              NES/EMULATOR-US-PER-OP
              (- NES/EMULATOR-US-PER-OP us-per-op)
              NES/CPU-US-PER-OP
              (- NES/CPU-US-PER-OP us-per-op))
     (list us-per-op NES/EMULATOR-US-PER-OP NES/CPU-US-PER-OP)))

(bench-rom 100 5000
           (f-join (file-name-directory buffer-file-name) "roms" "nestest.nes")
           (lambda (c r p)
             (setf (nes/cpu-register->pc r) #xc000)
             (setf (nes/cpu-register->sr-interrupt r) t)
             (setf (nes/cpu-register->sr-reserved r) t)
             (setf (nes/cpu-register->sr-break r) nil)
             (setf (nes/ppu->line p) 0)))

(bench-rom 100 10000
           (f-join (file-name-directory buffer-file-name) "../roms" "smb.nes"))

(provide 'bench)

;; Local Variables:
;; coding: utf-8
;; End:
;;; bench.el ends here
