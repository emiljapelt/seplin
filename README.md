# INEX - NONFUNCTIONAL PROGRAMMING

The idea is to make a programming language with the following properties:
- VM based, and compiled
- At the top level you can define routines, constants, variables and packs
- Each definition can be marked as internal or external
  - internal means that the definition is only visible in the file
  - external means that the definition is available everywhere
- Each file is a namespace, with the name of the file
- To use definitions from another file, use 'bridge "file_path"'
- When running the program, a routine name must be provided along with the arguments that it takes, in corresponding syntax
- Routines do not return anything, they only cause side-effects
- The stack is only for pointers to the heaps, and computations
- There are two heaps, one for packs and one for primitives and arrays
- Heap allocations can only be unbound in the routine that allocated it
  - And when a routine stops, it unbinds all allocations it made
- Local scopes, such as loop bodies, might need to be their own stack frame
- Definition hoisting would be nice to have
- Virtual Machine could be written in C, retrofitting my own malloc
- Compiler will be written in OCaml
- Source code in .ix files, compiled code in .ixc

Here is an example program, along with some explanation
```
external routine Start(int x) {
    int steps := 5;
    int result := 0;
    Fib(steps, result, 1);
    print result;
}

internal routine Fib(int step, int prev, int curr) {
    if (step = 0) stop;
    else {
        int temp := curr;
        curr := prev + temp;
        prev := temp;
        unbind temp;
        step := step-1;
        Fib(step, prev, curr);
    }
}
```

1. Defines an external routine called 'Start', which takes an integer reference
2. A number 5 is allocated to the primitive heap
3. A number 0 is allocated to the primitive heap
4. Allocates a number 1 to the primitive heap, calls the routine 'Fib', giving it references to step, result, and the allocated number. Unbind the number 1, when the routine stops.
5. Prints the value located at result
6. Unbinds step and result
7. N/A
8. Defines an internal routine called 'Fib', which takes 3 integer references
9. Loads 0 and the value at 'step' to the stack, and compares them, if equal stop the routine
10. else branch
11. A number with the value at 'curr' is allocated to the primitive heap
12. Loads the value at 'temp' and 'prev' to the stack, adds them, and assigns the result to 'curr'
13. Loads the value at 'temp' to the stack, and assigns it to 'prev'
14. Unbinds 'temp'
15. Loads the value at 'step' and 1 to the stack, substracts them, and assigns the result to 'step'
16. Calls the routine Fib, giving it references to step, prev and curr