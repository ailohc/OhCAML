# OhCAML

OhCAML is Checking Assistant for ML

## Install Package
Install package with opam
```
$ opam install z3
```

## Build
```
$ ./build
```

## Run
```
Usage: main.native <options> <file>

<options description>
--help                help
--run <file>          print result of symbolic execution
--criteria <file>     compare with 'target' file
--target <file>       compare with 'criteria' file
--counter             make counter example that make different output
```
For example,
```
$ ./main.native --run testcase/quadruple.m
```

## Clean
```
$ ./build clean
```
