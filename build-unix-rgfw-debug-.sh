#!/bin/sh
mlton -const 'Exn.keepHistory true' -link-opt "-lX11 -lXrandr -lGL -lm" \
  -export-header ffi/export.h \
  shf-tests.mlb \
  ffi/rgfw-export.c
