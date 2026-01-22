#include <math.h>
#define RGFW_IMPLEMENTATION
#define RGFW_OPENGL /* if this line is not added, OpenGL functions will not be included */
#include "RGFW.h"

#ifdef RGFW_MACOS
#include <OpenGL/gl.h> /* why does macOS do this */
#else
#include <GL/gl.h>
#endif

RGFW_windowFlags OPENGL_WINDOW = RGFW_windowOpenGL;

RGFW_window* createWindow(char* title, int x, int y, int width, int height, RGFW_windowFlags options) {
  return RGFW_createWindow(title, x, y, width, height, options);
}
