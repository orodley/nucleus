#!/usr/bin/env python2

import collections
import fnmatch
import itertools
import multiprocessing
import os
import subprocess
import sys
import time
import uuid

def main():
    if len(sys.argv) > 1:
        tests = sys.argv[1:]
    else:
        tests = []
        for root, dirnames, filenames in os.walk('tests'):
            for filename in fnmatch.filter(filenames, '*.nuc'):
                tests.append(os.path.join(root, filename))

    num_tests = len(tests)
    os.chdir(os.path.dirname(os.path.abspath(__file__)))

    print "Running %d tests:" % num_tests

    # Try to keep n processes running at once, where n = the number of cpus on
    # this machine.
    # NOTE: we don't bother running the binaries produced in parallel, because
    # none of the tests we have so far take a significant amount of time to run.
    # If they start taking a while, we'll need to change this.
    parallel_nucc_procs = multiprocessing.cpu_count()
    running_nucc_procs = []
    results = []
    while len(results) != num_tests:
        done, running_nucc_procs = partition(running_nucc_procs,
                lambda result: result['nucc_proc'].poll() is not None)

        while len(running_nucc_procs) < parallel_nucc_procs and len(tests) > 0:
            new_test = tests.pop()
            running_nucc_procs.append(start_compiling(new_test))

        if len(done) == 0:
            time.sleep(0.05)

        for test in done:
            result = run_test(test)
            sys.stdout.write(result_char(result))
            sys.stdout.flush()
            results.append(result)

    print "\n"

    passes = sum(1 for result in results if result['passed'])
    print "%d / %d tests passed" % (passes, num_tests)

    for result in results:
        if not result['passed']:
            print "\ntest '%s' failed:\n%s" % (result['name'], result['error'])

def start_compiling(test_filename):
    result = {'name': test_filename}

    with open(test_filename, 'r') as f:
        process_header(result, f)

    result['binary'] = "%s_%s" % (test_filename, str(uuid.uuid4()))
    nucc_proc = subprocess.Popen(["boot/nucc.sh", test_filename, result['binary']],
        stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    result['nucc_proc'] = nucc_proc
    return result

def run_test(result):
    nucc_proc = result['nucc_proc']
    result['compile-stdout'], result['compile-stderr'] = nucc_proc.communicate()
    result['compiled'] = nucc_proc.returncode == 0
    if result['expected-compile-stderr'] != '':
        # We use a fuzzy match as compile errors can be huge
        if not result['expected-compile-stderr'] in result['compile-stderr']:
            result['passed'] = False
            if result['compiled']:
                os.remove(result['binary'])
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

    program_proc = subprocess.Popen(["./" + result['binary']],
        stdout=subprocess.PIPE, stderr=subprocess.PIPE, stdin=subprocess.PIPE)
    result['run-stdout'], result['run-stderr'] = \
        program_proc.communicate(result['stdin'])
    result['status-code'] = program_proc.returncode
    os.remove(result['binary'])

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

def process_header(result, f):
    lines = []
    for line in f.readlines():
        if line[0] == ';':
            lines.append(line)
        else:
            break
    result['expected-status-code'] = 0
    result['expected-run-stdout'] = ''
    result['expected-compile-stderr'] = ''
    result['stdin'] = ''
    for line in lines:
        line = line[1:].strip()
        expectation_type, expectation = [cmpt.strip() for cmpt in line.split(':')]

        if expectation_type == 'status-code':
            result['expected-status-code'] = int(expectation)
        elif expectation_type == 'run-stdout':
            result['expected-run-stdout'] = escape_str(expectation)
        elif expectation_type == 'compile-stderr':
            result['expected-compile-stderr'] = escape_str(expectation)
        elif expectation_type == 'stdin':
            result['stdin'] = escape_str(expectation)

def escape_str(s):
    return s.replace('\\n', '\n')

def indent(str):
    return '\n'.join("    " + line for line in str.split('\n'))

def result_char(result):
    if result['passed']:
        return '.'
    else:
        return 'F'

def partition(l, pred):
    true = []
    false = []
    for x in l:
        if pred(x):
            true.append(x)
        else:
            false.append(x)

    return true, false

if __name__ == "__main__":
    main()
