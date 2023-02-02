;;; nes-keypad.el --- summary -*- lexical-binding: t -*-

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

(eval-when-compile (require 'cl-lib))

(defvar nes/keypad:a (kbd "d"))
(defvar nes/keypad:b (kbd "f"))
(defvar nes/keypad:up (kbd "<up>"))
(defvar nes/keypad:down (kbd "<down>"))
(defvar nes/keypad:left (kbd "<left>"))
(defvar nes/keypad:right (kbd "<right>"))
(defvar nes/keypad:select (kbd "SPC"))
(defvar nes/keypad:start (kbd "RET"))

(cl-defstruct (nes/keypad
               (:conc-name nes/keypad->))
  (index 0)
  (set-flag nil)
  (buffers (make-vector #x10 0)) ;; 1P + 2P
  (copies (make-vector #x10 0)))

(defun nes/keypad-write (k value)
  (cond
   ((eq (logand #b01 value) 1)
    (setf (nes/keypad->set-flag k) t))

   ((nes/keypad->set-flag k)
    (progn
      (setf (nes/keypad->set-flag k) nil)
      (setf (nes/keypad->copies k) (copy-sequence (nes/keypad->buffers k)))
      (setf (nes/keypad->index k) 0)
      (fillarray (nes/keypad->buffers k) 0)))
   ))

(defun nes/keypad-read (k)
  (let ((value (aref (nes/keypad->copies k) (nes/keypad->index k))))
    (setf (nes/keypad->index k) (mod (1+ (nes/keypad->index k)) #x10))
    value))

(defun nes/keypad-check (k keycode)
  (aset (nes/keypad->buffers k) keycode 1))

(defun nes/keypad-init (k map)
  (define-key map nes/keypad:a      (lambda () (interactive) (nes/keypad-check k 0)))
  (define-key map nes/keypad:b      (lambda () (interactive) (nes/keypad-check k 1)))
  (define-key map nes/keypad:select (lambda () (interactive) (nes/keypad-check k 2)))
  (define-key map nes/keypad:start  (lambda () (interactive) (nes/keypad-check k 3)))
  (define-key map nes/keypad:up     (lambda () (interactive) (nes/keypad-check k 4)))
  (define-key map nes/keypad:down   (lambda () (interactive) (nes/keypad-check k 5)))
  (define-key map nes/keypad:left   (lambda () (interactive) (nes/keypad-check k 6)))
  (define-key map nes/keypad:right  (lambda () (interactive) (nes/keypad-check k 7)))
  )

(provide 'nes-keypad)

;; Local Variables:
;; coding: utf-8
;; End:
;;; nes-keypad.el ends here
