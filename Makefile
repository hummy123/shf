run_debug:
	./build-unix-debug.sh && ./shf

run:
	./build-unix.sh && ./shf

rgfw:
	./build-unix-rgfw-debug-.sh && ./shf-rgfw

tests:
	mlton -const "Exn.keepHistory true" shf-tests.mlb && ./shf-tests
