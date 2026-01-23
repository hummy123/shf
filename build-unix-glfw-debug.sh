#!/bin/sh
mlton -const 'Exn.keepHistory true' -link-opt "$(pkg-config --cflags glfw3) $(pkg-config --static --libs glfw3)" \
  -export-header ffi/mlton-glfw-export.h \
  shf-glfw.mlb \
  ffi/glad.c \
  ffi/glfw-export.c \
  ffi/glfw-input.c
