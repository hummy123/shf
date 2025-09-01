#include "export.h"
#include "glad.h"
#define GLFW_INCLUDE_NONE
#include <GLFW/glfw3.h>

int PRESS = GLFW_PRESS;
int REPEAT = GLFW_REPEAT;
int RELEASE = GLFW_RELEASE;
int KEY_ESC = GLFW_KEY_ESCAPE;
int KEY_ENTER = GLFW_KEY_ENTER;
int KEY_BACKSPACE = GLFW_KEY_BACKSPACE;

int KEY_ARROW_LEFT = GLFW_KEY_LEFT;
int KEY_ARROW_RIGHT = GLFW_KEY_RIGHT;
int KEY_ARROW_UP = GLFW_KEY_UP;
int KEY_ARROW_DOWN = GLFW_KEY_DOWN;

void framebufferSizeCallback(GLFWwindow* window, int width, int height) {
  glViewport(0, 0, width, height);
  mltonFramebufferSizeCallback(width, height);
}

void setFramebufferSizeCallback(GLFWwindow* window) {
  glfwSetFramebufferSizeCallback(window, framebufferSizeCallback);
}

void charCallback(GLFWwindow* window, unsigned int codepoint) {
  mltonCharCallback(codepoint);
}

void setCharCallback(GLFWwindow* window) {
  glfwSetCharCallback(window, charCallback);
}

void keyCallback(GLFWwindow *window, int key, int scancode, int action, int mods) {
  mltonKeyCallback(key, scancode, action, mods);
}

void setKeyCallback(GLFWwindow *window) {
  glfwSetKeyCallback(window, keyCallback);
}

