#define RGFW_OPENGL
#define RGFW_ALLOC_DROPFILES
#define RGFW_IMPLEMENTATION
#define RGFW_PRINT_ERRORS
#define RGFW_DEBUG
#define GL_SILENCE_DEPRECATION
#include "RGFW.h"
#include <GLES3/gl3.h>
#include <stdbool.h>
#include <ctype.h>
#include "mlton-rgfw-export.h"

RGFW_window* createWindow(char* title, int x, int y, int width, int height) {
  return RGFW_createWindow(title, x, y, width, height, RGFW_windowCenter | RGFW_windowOpenGL);
}

void closeWindow(RGFW_window* window) {
  RGFW_window_close(window);
}

Bool shouldCloseWindow(RGFW_window* window) {
  if (RGFW_window_shouldClose(window)) {
    return true;
  } else {
    return false;
  }
}

void swapBuffers(RGFW_window* window) {
  RGFW_window_swapBuffers_OpenGL(window);
}

void writeClipboard(char* string, int stringSize) {
  RGFW_writeClipboard(string, stringSize);
}

void keyCallback(RGFW_window* window, unsigned char key, unsigned char symbol, unsigned char keymod, unsigned char repeated, unsigned char pressed) {
  if (pressed || repeated) {
    switch (key) { 
      case RGFW_escape:
	mltonEscape();
	break;
      case RGFW_backSpace:
	mltonBackspace();
	break;
      case RGFW_enter:
        mltonEnter();
        break;

      case RGFW_backtick:
      case RGFW_0:
      case RGFW_1:
      case RGFW_2:
      case RGFW_3:
      case RGFW_4:
      case RGFW_5:
      case RGFW_6:
      case RGFW_7:
      case RGFW_8:
      case RGFW_9:
      case RGFW_minus:
      case RGFW_equal:
      case RGFW_tab:
      case RGFW_space:
      case RGFW_a:
      case RGFW_b:
      case RGFW_c:
      case RGFW_d:
      case RGFW_e:
      case RGFW_f:
      case RGFW_g:
      case RGFW_h:
      case RGFW_i:
      case RGFW_j:
      case RGFW_k:
      case RGFW_l:
      case RGFW_m:
      case RGFW_n:
      case RGFW_o:
      case RGFW_p:
      case RGFW_q:
      case RGFW_r:
      case RGFW_s:
      case RGFW_t:
      case RGFW_u:
      case RGFW_v:
      case RGFW_w:
      case RGFW_x:
      case RGFW_y:
      case RGFW_z:
      case RGFW_period:
      case RGFW_comma:
      case RGFW_slash:
      case RGFW_bracket:
      case RGFW_closeBracket:
      case RGFW_semicolon:
      case RGFW_apostrophe:
      case RGFW_backSlash:
	if (keymod == RGFW_modShift) {
	  mltonChar(toupper((char)key));
	  break;
	} else {
	  mltonChar((char)key);
	  break;
	}
    }
  }
}

void setKeyCallback() {
  RGFW_setKeyCallback(keyCallback);
}

void resizeCallback(RGFW_window* window, int width, int height) {
  glViewport(0, 0, width, height);
  mltonResize(width, height);
}

void setResizeCallback() {
  RGFW_setWindowResizedCallback(resizeCallback);
}

void pollEvents() {
  RGFW_pollEvents();
}

// OpenGL constants used below
unsigned int VERTEX_SHADER = GL_VERTEX_SHADER;
unsigned int FRAGMENT_SHADER = GL_FRAGMENT_SHADER;
unsigned int TRIANGLES = GL_TRIANGLES;
unsigned int STATIC_DRAW = GL_STATIC_DRAW;
unsigned int DYNAMIC_DRAW = GL_DYNAMIC_DRAW;

// OpenGL functions used below
void enableDepthTest() {
  glEnable(GL_DEPTH_TEST);
}

void viewport(int width, int height) {
  glViewport(0, 0, width, height);
}

void clearColor(float r, float g, float b, float a) {
  glClearColor(r, g, b, a);
}

void clear() {
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
}

unsigned int createBuffer() {
  unsigned int buffer;
  glGenBuffers(1, &buffer);
  return buffer;
}

void bindBuffer(unsigned int buffer) {
  glBindBuffer(GL_ARRAY_BUFFER, buffer);
}

void bufferData(float* vector, int vectorLength, unsigned int updateMode) {
  glBufferData(GL_ARRAY_BUFFER, sizeof(float) * vectorLength, vector, updateMode);
}

unsigned int createShader(unsigned int shaderType) {
  return glCreateShader(shaderType);
}

void shaderSource(unsigned int shader, const char *sourceString) {
  glShaderSource(shader, 1, &sourceString, NULL);
}

void compileShader(unsigned int shader) {
  glCompileShader(shader);
}

void deleteShader(unsigned int shader) {
  glDeleteShader(shader);
}

void vertexAttribPointer(int location, int numVecComponents, int stride, int offset) {
  glVertexAttribPointer(location, numVecComponents, GL_FLOAT, GL_FALSE, stride * sizeof(float), (void*)offset);
}

void enableVertexAttribArray(int location) {
  glEnableVertexAttribArray(location);
}

unsigned int createProgram() {
  return glCreateProgram();
}

void attachShader(unsigned int program, unsigned int shader) {
  glAttachShader(program, shader);
}

void linkProgram(unsigned int program) {
  glLinkProgram(program);
}

void useProgram(unsigned int program) {
  glUseProgram(program);
}

void deleteProgram(unsigned int program) {
  glDeleteProgram(program);
}

void drawArrays(unsigned int drawMode, int startIndex, int numVertices) {
  glDrawArrays(drawMode, startIndex, numVertices);
}

int getUniformLocation(unsigned int program, const char *uniformName) {
  glGetUniformLocation(program, uniformName);
}

void uniform4f(int uniformLocation, float a, float b, float c, float d) {
  glUniform4f(uniformLocation, a, b, c, d);
}
