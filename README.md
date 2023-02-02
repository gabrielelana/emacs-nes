# A NES Emulator

[![https://img.shields.io/badge/%E2%9D%A4%EF%B8%8F-open%20source%20saturday-F64060.svg](https://img.shields.io/badge/%E2%9D%A4%EF%B8%8F-open%20source%20saturday-F64060.svg)](https://www.meetup.com/it-IT/Open-Source-Saturday-Milano/)

A NES Emulator written in Emacs Lisp running in Emacs

NOTE: this is based on the original work of https://github.com/gongo/emacs-nes

## Install

It's not intended to be used yet (too buggy, too slow) but if you want, in a
recent version of Emacs (I will be more precise in the future when we will have
tests) do one of the following

### Straight

```cl
(use-package emacs-nes
  :straight (:host github
             :repo "gabrielelana/emacs-nes"
             :files ("*.el")))
```

### Manual

Copy all `*.el` files in the top level directory of this repository somewhere on
the load path

## Usage

It's not intended to be used yet (too buggy, too slow) but if you want, in a
recent version of Emacs (I will be more precise in the future when we will have
tests) follow this steps:

- Install [retro.el](https://github.com/gabrielelana/retro.el)
- Install the code in this repository, see [Install](#install)
- Execute the following function
  ```cl
  (nes "path/to/rom.nes")
  ```

## History

Everything started when I wanted (for no reason) to play [dino race](chrome://dino) in Emacs, this
led to [retro.el](https://github.com/gabrielelana/retro.el) a render and (primitive) game engine
written in Emacs Lisp and running in Emacs capable of reaching ~30fps for a VGA resolution (640x480).

Pumped for the achievement I wanted to do more and searched the internet for inspiration, emulation
seemed a good fit and I came across a [NES emulator already written in Emacs
Lisp](https://github.com/gongo/emacs-nes) which was already capable of running "Super Mario Bros"
with a few glitches but was extremely slow.

I was able to integrate [retro.el](https://github.com/gabrielelana/retro.el) and obtained a
significant (10x) speed up but it wasn't enough to make it playable.

Then the journey begun to make it correct and performant as possible.

## Self-Promotion

If you like this project, then consider to:

- Star the repository on [GitHub](https://github.com/gabrielelana/emacs-nes)
- Follow me on
  - [Twitter](http://twitter.com/gabrielelana)
  - [GitHub](https://github.com/gabrielelana)
