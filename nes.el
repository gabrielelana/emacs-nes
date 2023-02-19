;;; nes.el --- A NES Emulator written in Emacs Lisp running in Emacs -*- lexical-binding: t -*-

;; Author: Gabriele Lana <gabriele.lana@gmail.com>
;; Maintainer: Gabriele Lana <gabriele.lana@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "28.1") (ht "2.0.0") (retro.el "0.0.1"))
;; Homepage: http://github.com/gabrielelana/nes.el
;; Keywords: NES, emulator

;; This is a fork of emacs-nes.el wrote by Wataru MIYAGUNI.

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

;; Library to create retro games in Emacs.

;;; Code:

(eval-when-compile (require 'cl-lib))

(require 'nes-cartridge)
(require 'nes-ppu)
(require 'nes-cpu)
(require 'nes-dma)
(require 'nes-keypad)
(require 'nes-instruction)
(require 'nes-interrupt)
(require 'nes-color)
(require 'nes-util)

(require 'retro)

(defconst nes-buffer-name "*NES-EMULATOR*")

(defvar nes--current-cartridge-filename nil)
(defvar nes--current-game nil)

(setq nes-mode-map
      (let ((map (make-sparse-keymap 'nes-mode-map)))
        (define-key map (kbd "q") #'nes-quit)
        map))

(cl-defstruct nes
  (cpu nil)
  (ppu nil)
  (dma nil)
  (keypad nil)
  (cart nil)
  (interrupt nil)
  (canvas nil))

(defun nes-setup (filename switch-to-buffer-p)
  (let ((cart (nes/cartridge-load filename))
        (keypad (make-nes/keypad))
        (interrupt (make-nes/interrupt))
        (ram (make-vector #x0800 0))
        cpu
        ppu
        dma
        canvas)
    ;; cavas
    (setq canvas (nes/ppu-init nes-buffer-name switch-to-buffer-p))

    ;; ppu
    (setq ppu (make-nes/ppu
               :interrupt interrupt
               :current-canvas canvas
               :previous-canvas (retro-canvas-copy canvas)))
    (nes/ppu-set-character-ram ppu (copy-sequence (nes/cartridge->chr-rom cart)))

    ;; dma
    (setq dma (make-nes/dma :ppu ppu :ram ram))

    ;; keypad
    (nes/keypad-init keypad nes-mode-map)

    ;; cpu
    (setq cpu (make-nes/cpu
               :ppu ppu
               :keypad keypad
               :interrupt interrupt
               :dma dma))
    (nes/cpu-set-working-ram cpu ram)
    (nes/cpu-set-program-rom cpu (let ((cart cart))
                                   (lambda (addr)
                                     (nes/cartridge-read-from-prg-rom cart addr))))
    (nes/cpu-init cpu)

    (make-nes
     :cpu cpu
     :ppu ppu
     :dma dma
     :keypad keypad
     :cart cart
     :interrupt interrupt
     :canvas canvas)))

(defun nes-update ()
  (let ((buffer (get-buffer nes-buffer-name)))
    (when (eq (current-buffer) buffer)
      (let ((c (nes-cpu nes--current-game))
            (p (nes-ppu nes--current-game))
            (d (nes-dma nes--current-game)))
        (nes/dma-transfer d)
        (dotimes (_ 3000)
          (dotimes (_ (* (nes/cpu-step c) 3))
            (nes/ppu-step p)))))
    (when buffer
      (run-at-time 0.001 nil 'nes-update))))

(defun nes-quit ()
  (interactive)
  (setq nes--current-game nil)
  (kill-buffer nes-buffer-name))

(define-derived-mode nes-mode nil "NES Emulator"
  (use-local-map nes-mode-map)
  (setq nes--current-game (nes-setup nes--current-cartridge-filename t))
  (run-at-time 0.001 nil 'nes-update))

(defun nes (filename)
  "nes-mode keybindings:
\\{nes-mode-map}"
  (interactive "ffilename: ")
  (select-window (or (get-buffer-window nes-buffer-name)
                     (selected-window)))
  (switch-to-buffer nes-buffer-name)
  (setq nes--current-cartridge-filename filename)
  (nes-mode))

(provide 'nes)

;; Local Variables:
;; coding: utf-8
;; End:
;;; nes.el ends here
