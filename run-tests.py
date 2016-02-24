#!/usr/bin/env python2

import fnmatch
import multiprocessing
import os
import subprocess
import signal
import sys
import time
import uuid

def main():
    nucc = './nucc'
    print_details = True
    be_positive = False
    tests = []
    for arg in sys.argv[1:]:
        if arg == '-s':
            print_details = False
        elif arg == '-p':
            be_positive = True
        else:
            tests.append(arg)
    if not print_details and be_positive:
        print "Cannot specify '-s' and '-p'"
        return

    if tests == []:
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
            running_nucc_procs.append(start_compiling(new_test, nucc))

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

    if print_details:
        for result in results:
            if be_positive:
                if result['passed']:
                    print "test '%s' passed" % result['name']
            else:
                if not result['passed']:
                    print "\ntest '%s' failed:\n%s" % \
                            (result['name'], result['error'])

def start_compiling(test_filename, nucc):
    result = {'name': test_filename}

    with open(test_filename, 'r') as f:
        process_header(result, f)

    result['binary'] = "%s_%s.tmp" % (test_filename, str(uuid.uuid4()))
    nucc_proc = subprocess.Popen([nucc, test_filename, result['binary']],
        stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    result['nucc_proc'] = nucc_proc
    return result

def run_test(result):
    nucc_proc = result['nucc_proc']
    # TODO: This seems to hang forever if nucc produces loads of output (like
    # if we have some debugging stuff in there.
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

    binary_path = os.path.abspath(result['binary'])
    test_dir = os.path.abspath(os.path.dirname(result['name']))
    program_proc = subprocess.Popen(binary_path,
        stdout=subprocess.PIPE, stderr=subprocess.PIPE, stdin=subprocess.PIPE,
        cwd=test_dir)
    result['run-stdout'], result['run-stderr'] = \
        program_proc.communicate(result['stdin'])
    result['status-code'] = program_proc.returncode
    os.remove(binary_path)

    result['passed'] = True
    if result['status-code'] != result['expected-status-code']:
        result['passed'] = False
        result['error'] = "expected return code %d, got %d" % \
            (result['expected-status-code'], result['status-code'])
    if result['run-stdout'] != result['expected-run-stdout']:
        result['passed'] = False
        result['error'] = "expected runtime stdout of %r, got %r" % \
            (result['expected-run-stdout'], result['run-stdout'])
    if result['run-stderr'] != result['expected-run-stderr']:
        result['passed'] = False
        result['error'] = "expected runtime stderr of %r, got %r" % \
            (result['expected-run-stderr'], result['run-stderr'])

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
    result['expected-run-stderr'] = ''
    result['expected-compile-stderr'] = ''
    result['stdin'] = ''
    for line in lines:
        line = line[1:].strip()
        expectation_type, expectation = [cmpt.strip() for cmpt in line.split(':')]

        if expectation_type == 'status-code':
            result['expected-status-code'] = int(expectation)
        elif expectation_type == 'run-stdout':
            result['expected-run-stdout'] = escape_str(expectation)
        elif expectation_type == 'run-stderr':
            result['expected-run-stderr'] = escape_str(expectation)
        elif expectation_type == 'compile-stderr':
            result['expected-compile-stderr'] = escape_str(expectation)
        elif expectation_type == 'stdin':
            result['stdin'] = escape_str(expectation)

def escape_str(s):
    return s.replace('\\n', '\n')

def indent(str):
    return '\n'.join("    " + line for line in str.split('\n'))

# TODO: check if we're on a platform that doesn't support ANSI colors
green = '\033[92m'
red = '\033[91m'
reset_color = '\033[0m'

def result_char(result):
    if result['passed']:
        return green + '.' + reset_color
    else:
        return red + 'F' + reset_color

def partition(l, pred):
    true = []
    false = []
    for x in l:
        if pred(x):
            true.append(x)
        else:
            false.append(x)

    return true, false

def sigint_handler(signal, frame):
    # If we get Ctrl-C in between finishing compiling a test and running it,
    # the binary is still there.
    for root, dirnames, filenames in os.walk('tests'):
        for filename in fnmatch.filter(filenames, '*.tmp'):
            os.remove(os.path.join(root, filename))

    sys.exit(0)

if __name__ == "__main__":
    signal.signal(signal.SIGINT, sigint_handler)
    main()
