run:
	./build-unix.sh && ./shf

tests:
	mlton shf-tests.mlb && ./shf-tests
