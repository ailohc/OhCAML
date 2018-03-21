# OhCAML

OhCAML is Checking Assistant for ML

## Install Package
```
$ opam install z3
```

## Build
```
$ ./build
```

## Run
Usage: main.native <options> <file>

options description
-h                    help
--run <file>          print result of symbolic execution
--criteria <file>     compare with 'target' file
--target <file>       compare with 'criteria' file
--counter             make counter example that make different output
  
```
$ ./main.native --run testcase/quadruple.m
```

## Clean
```
$ ./build clean
```
