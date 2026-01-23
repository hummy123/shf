#!/bin/sh
mlton -const 'Exn.keepHistory true' -link-opt "-lX11 -lXrandr -lGL" \
  -export-header ffi/mlton-rgfw-export.h \
  shf-rgfw.mlb \
  ffi/rgfw-export.c
