run_debug:
	./build-unix-debug.sh && ./shf

run:
	./build-unix.sh && ./shf

tests:
	mlton shf-tests.mlb && ./shf-tests
