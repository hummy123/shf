#!/bin/sh
mlton -const 'Exn.keepHistory true' -link-opt "-lX11 -lXrandr -lGL" \
  shf-rgfw.mlb \
  ffi/rgfw-export.c
