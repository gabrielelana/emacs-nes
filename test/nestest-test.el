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

(require 'nes)

(ert-deftest nestest-shall-pass ()
  (let ((emulator (nes-setup "/home/coder/code/emacs-nes/test/roms/nestest.nes" nil)))
    (should emulator))

    ;; this.emulator.cpu.reset();
    ;; this.emulator.cpu.registers.PC = 0xC000;
    ;; this.emulator.cpu.deferCycles = 7;
    ;; this.emulator.cpu.setFlag(Flags.I, true);
    ;; this.emulator.cpu.setFlag(Flags.U, true);
    ;; this.emulator.ppu.scanLine = 0;
    ;; this.emulator.ppu.cycle = -21;
  )

(provide 'example-test)

;; Local Variables:
;; coding: utf-8
;; End:
;;; nestest-test.el ends here
