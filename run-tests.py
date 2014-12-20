#!/usr/bin/env python2

import collections
import os
import subprocess
import sys
import uuid

def main():
    os.chdir(os.path.dirname(os.path.abspath(__file__)))

    results = []
    tests = os.listdir("tests")
    print "Running %d tests:" % len(tests)

    for test_file in tests:
        test_file = "tests/" + test_file
        result = run_test(test_file)
        sys.stdout.write(result_char(result))
        sys.stdout.flush()
        results.append(result)

    print "\n"

    passes = sum(1 for result in results if result['passed'])
    print "%d / %d tests passed" % (passes, len(tests))

    for result in results:
        if not result['passed']:
            print "\ntest '%s' failed:" % result['name']

            if not result['compiled']:
                print "compilation failed with stderr:"
                print '\n'.join("    " + line for line in
                        result['compile_stderr'].split('\n'))
            else:
                print "expected return code %d, got %d" % \
                    (result['expected_return_code'], result['return_code'])


def run_test(test_file):
    result = {'name': test_file}

    temp_file = str(uuid.uuid4())
    nucc_proc = subprocess.Popen(["boot/nucc.sh", test_file, temp_file],
            stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    result['compile_stdout'], result['compile_stderr'] = nucc_proc.communicate()
    result['compiled'] = nucc_proc.returncode == 0
    if not result['compiled']:
        result['passed'] = False
        return result

    program_proc = subprocess.Popen(["./" + temp_file],
            stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    result['run_stdout'], result['run_stderr'] = program_proc.communicate()
    result['return_code'] = program_proc.returncode
    os.remove(temp_file)

    with open(test_file, 'r') as f:
        result['expected_return_code'] = int(f.readline()[1:])
    if result['return_code'] != result['expected_return_code']:
        result['passed'] = False
        return result

    result['passed'] = True
    return result

def result_char(result):
    if result['passed']:
        return '.'
    else:
        return 'F'

if __name__ == "__main__":
    main()
