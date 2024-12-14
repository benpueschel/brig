#!/usr/bin/env sh

tests="$(find test -name '*.brig' -type f)"
failed=0
for test in $tests; do
	echo "Running test $test"
	cargo run --quiet -- "$test" 2>/dev/null 1>/dev/null
	status="$?"
	if [ $status -ne 0 ]; then
		printf "  failed with status %d\n" $status
		failed+=1
	else
		printf "  passed\n" $test
	fi
	echo ""
done

echo "Testing C interop"
echo ""
./test.sh

if [ $? -ne 0 ]; then
	echo "C interop tests failed"
	failed+=1
fi

if [ $failed -ne 0 ]; then
	printf "%s tests failed\n" $failed
	exit 1
fi

echo ""
echo "All tests passed"
