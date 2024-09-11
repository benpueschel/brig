#!/usr/bin/env sh

mkdir -p .brig.tmp \
	&& cargo run -- -o .brig.tmp/test.a test/test.brig \
	&& as .brig.tmp/test.a -o .brig.tmp/test.o \
	&& gcc -o ctest test/ctest.c .brig.tmp/test.o \
	&& rm -rf .brig.tmp \
	&& ./ctest
