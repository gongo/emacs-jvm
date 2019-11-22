#!/bin/sh
":"; exec emacs --quick --script "$0" "$@" # -*- mode: emacs-lisp; lexical-binding: t; -*-

(let ((current-dir (file-name-directory (or load-file-name (buffer-file-name)))))
  (add-to-list 'load-path current-dir))

(require 'jvm-classfile)

(let* ((classfile (jvm--classfile-load (nth 0 command-line-args-left)))
       (method (jvm--classfile-find-method classfile "main")))
  (apply #'jvm--classfile-run-method method (cl-subseq command-line-args-left 1)))
