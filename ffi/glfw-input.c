#include "export.h"
#include "glad.h"
#define GLFW_INCLUDE_NONE
#include <GLFW/glfw3.h>

int PRESS = GLFW_PRESS;
int REPEAT = GLFW_REPEAT;
int RELEASE = GLFW_RELEASE;

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

