rgfw-debug:
	./build-unix-rgfw-debug-.sh && ./shf-rgfw

glfw-debug:
	./build-unix-glfw-debug.sh && ./shf-glfw

glfw:
	./build-unix-glfw.sh && ./shf-glfw

tests:
	mlton -const "Exn.keepHistory true" shf-tests.mlb && ./shf-tests
