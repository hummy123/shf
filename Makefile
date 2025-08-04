run_debug:
	./build-unix-debug.sh && ./shf

run:
	./build-unix.sh && ./shf

tests:
	mlton -const "Exn.keepHistory true" shf-tests.mlb && ./shf-tests
