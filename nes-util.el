;; -*- lexical-binding: t -*-

(defsubst nes--logbitp (index byte)
  (declare (pure t) (side-effect-free t))
  (/= (logand byte (ash 1 index)) 0))

(provide 'nes-util)
