#!/usr/bin/env python2

import collections
import fnmatch
import os
import subprocess
import sys
import uuid

def main():
    if len(sys.argv) > 1:
        tests = sys.argv[1:]
    else:
        tests = []
        for root, dirnames, filenames in os.walk('tests'):
            for filename in fnmatch.filter(filenames, '*.nuc'):
                tests.append(os.path.join(root, filename))

    os.chdir(os.path.dirname(os.path.abspath(__file__)))

    results = []
    print "Running %d tests:" % len(tests)

    for test_file in tests:
        result = run_test(test_file)
        sys.stdout.write(result_char(result))
        sys.stdout.flush()
        results.append(result)

    print "\n"

    passes = sum(1 for result in results if result['passed'])
    print "%d / %d tests passed" % (passes, len(tests))

    for result in results:
        if not result['passed']:
            print "\ntest '%s' failed:\n%s" % (result['name'], result['error'])

def run_test(test_file):
    result = {'name': test_file}

    with open(test_file, 'r') as f:
        add_expectations(result, f)

    temp_file = "%s_%s" % (test_file, str(uuid.uuid4()))
    nucc_proc = subprocess.Popen(["boot/nucc.sh", test_file, temp_file],
            stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    result['compile-stdout'], result['compile-stderr'] = nucc_proc.communicate()
    result['compiled'] = nucc_proc.returncode == 0
    if result['expected-compile-stderr'] != '':
        # We use a fuzzy match as compile errors can be huge
        if not result['expected-compile-stderr'] in result['compile-stderr']:
            result['passed'] = False
            if result['compiled']:
                os.remove(temp_file)
                result['error'] = "compilation succeeded when expected to fail"
            else:
                result['error'] = "expected compile stderr matching '%s', got:\n%s" \
                    % (result['expected-compile-stderr'],
                       indent(result['compile-stderr']))
            return result
        else:
            result['passed'] = True
            return result

    if not result['compiled']:
        result['passed'] = False
        result['error'] = "compilation failed with stderr:\n" \
            + indent(result['compile-stderr'])
        return result

    program_proc = subprocess.Popen(["./" + temp_file],
            stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    result['run-stdout'], result['run-stderr'] = program_proc.communicate()
    result['status-code'] = program_proc.returncode
    os.remove(temp_file)

    if result['run-stderr'] != '':
        result['passed'] = False;
        result['error'] = "non-empty stderr at runtime:\n" \
            + indent(result['run-stderr'])
        return result

    result['passed'] = True
    if result['status-code'] != result['expected-status-code']:
        result['passed'] = False
        result['error'] = "expected return code %d, got %d" % \
            (result['expected-status-code'], result['status-code'])
    if result['run-stdout'] != result['expected-run-stdout']:
        result['passed'] = False
        result['error'] = "expected runtime stdout of %r, got %r" % \
            (result['expected-run-stdout'], result['run-stdout'])

    return result

def add_expectations(result, f):
    lines = []
    for line in f.readlines():
        if line[0] == ';':
            lines.append(line)
        else:
            break
    result['expected-status-code'] = 0
    result['expected-run-stdout'] = ''
    result['expected-compile-stderr'] = ''
    for line in lines:
        line = line[1:].strip()
        expectation_type, expectation = [cmpt.strip() for cmpt in line.split(':')]

        if expectation_type == 'status-code':
            result['expected-status-code'] = int(expectation)
        elif expectation_type == 'run-stdout':
            result['expected-run-stdout'] = expectation.strip('"')
        elif expectation_type == 'compile-stderr':
            result['expected-compile-stderr'] = expectation.strip('"')

def indent(str):
    return '\n'.join("    " + line for line in str.split('\n'))

def result_char(result):
    if result['passed']:
        return '.'
    else:
        return 'F'

if __name__ == "__main__":
    main()
