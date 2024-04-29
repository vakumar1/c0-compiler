#!/bin/bash

# INTEGRATION TESTS
# usage: ./integration.sh
# --> reads all test files in test/integration/programs
#     compiles (in debug mode) and runs the program as an executable
#     and compares to the expected result
# *** the first line of each test program must be a comment with the expected output
#     e.g.: // Result: 5

programs_dir="test/integration/programs/"
work_dir="/tmp/c0_compiler/"

# create working directory
mkdir -p $work_dir

# compile compiler
stack build

run_test () {
  if [ -f $filepath ]; then
    file=$(basename $filepath)
    test=${file%.c}

    echo "Running test: $test"
    asm_file=$work_dir$test.asm
    object_file=$work_dir$test.o
    exec_file=$work_dir$test

    # run compiler and capture output of running final executable
    stack run $programs_dir$file debug > $asm_file
    nasm -f elf64 $asm_file -o $object_file
    ld $object_file -o $exec_file -m elf_x86_64
    actual_out=$($exec_file)

    # extract expected result and compare to actual output
    comment=$(head -n 1 $programs_dir$file)
    expected_out=${comment:3}
    if [ "$actual_out" == "$expected_out" ]; then
        echo "$test succeeded"

    else
        echo "$test FAILED"
    fi
    echo "  Extracted Result: $actual_out"
    echo "  Expected Result: $expected_out"
  else
    echo "Invalid file: $filepath"
  fi
}

if [ $# -eq 0 ]; then
   # run every test in the program dir
  for filepath in $programs_dir/*; do
    run_test $filepath
  done

else
  # run tests on provided files
  for filepath in "$@"; do
    run_test $filepath
  done

fi


rm -r $work_dir
