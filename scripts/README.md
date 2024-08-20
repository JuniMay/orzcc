# Useful Scripts

This folder contains some scripts for testing.

## hexfloat.c

A simple utility to see the binary representation of a floating point number.

```bash
gcc hexfloat.c -o hexfloat
./hexfloat 3.14
```

## execute.py

A script to run the compiler on a batch of source codes.

## build_table.py

A script to build a time table to compare diff outputs of the compiler.

## compare_result.py

A script to compare two diff row of the result csv file.

## merge_cases.py

A script to merge the testcases (function & performance) into one union file.

## merge_file.py

A script to merge more testcases using hash value ( tuple(hash(xx.in), hash(xx.sy), hash(xx.out)) ).

## extract_time_record.py

A script to extract the time record from the log file.