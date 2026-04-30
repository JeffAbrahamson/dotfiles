# Emacs Lisp

This directory contains the actual Emacs Lisp code loaded by the main Emacs configuration.

## Contents

* Core personal configuration: [`emacs.el`](emacs.el), [`jma.el`](jma.el), [`misc.el`](misc.el), [`mode-hooks.el`](mode-hooks.el), [`useful.el`](useful.el), and [`post.el`](post.el).
* Bundled modes or compatibility files: [`dockerfile-mode.el`](dockerfile-mode.el), [`protobuf-mode.el`](protobuf-mode.el), [`web-mode.el`](web-mode.el), [`google-c-style.el`](google-c-style.el), and related helpers.
* Focused utilities: [`mutt-alias.el`](mutt-alias.el), [`pdftools.el`](pdftools.el), [`calendar.el`](calendar.el), and [`dict.el`](dict.el).

## Notable files

* [`jma.el`](jma.el) and [`mode-hooks.el`](mode-hooks.el) are the main "local brain" of the setup, collecting custom behavior and mode-specific defaults in one place.
* [`useful.el`](useful.el) and [`misc.el`](misc.el) provide reusable helpers that keep the rest of the config from turning into repetitive glue code.
