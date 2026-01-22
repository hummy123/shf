#define RGFW_OPENGL
#define RGFW_ALLOC_DROPFILES
#define RGFW_IMPLEMENTATION
#define RGFW_PRINT_ERRORS
#define RGFW_DEBUG
#define GL_SILENCE_DEPRECATION
#include "RGFW.h"
#include <GLES3/gl3.h>
#include <stdbool.h>

RGFW_window* createWindow(char* title, int x, int y, int width, int height) {
  return RGFW_createWindow(title, x, y, width, height, RGFW_windowCenter | RGFW_windowOpenGL);
}

void closeWindow(RGFW_window* window) {
  RGFW_window_close(window);
}

bool shouldCloseWindow(RGFW_window* window) {
  return RGFW_window_shouldClose(window) != 0;
}

void swapBuffers(RGFW_window* window) {
  RGFW_window_swapBuffers_OpenGL(window);
}
