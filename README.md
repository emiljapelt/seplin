# INEX, a nonfunctional programming language

## Features
| Feature | Explaination |
| --- | --- |
| Side-effect oriented programming | Instead of functions/methods which can return some data, INEX used routines, which cannot return anything. However, all data is stored on the heap, and by default arguments are passed by reference, so changes made in a routine are persisted. |
| Full null'ability | Every variable can be null, by not pointing to any data on the heap.  |
| Full top-level hoisting | At the top-level order does not matter, every routine can use all global variables, all structs and all routines. The compiler will also resolve an ordering of global variables, such that circular dependencies will be detected. |
| Multi entry point | Routines are marked either 'internal' or 'external'. When running the program, any of the external routines can be used as the entry point. |
| Generic types | Both structs and routines can be generic. |
| Type inference | Both generic types, and the type of non-global variable declarations, can be infered.  |

## Types
| Type | Size | Default value |
| --- | --- | --- |
| int | 8B | 0 |
| char | 1B | not sure |
| bool | 1B | false |
| []  | size * 8B  | null |
| struct | fields * 8B | null|

## Variable declaration
Variables are declared the same way at the global and local level. Except for type inferece not being available at the global level. 

Variables can optionally be locked, meaning that it will not change after declaration. This is done with the 'locked' keyword.

```
g_number_0 : int;
g_number_1 : int := 1;
gl_number_0 : locked int;
gl_number_1 : locked := 1;

external main() {
    l_number_0 : int;
    l_number_1 : int := 1;
    l_number_2 ::= 1;
    ll_number_0 : locked int;
    ll_number_1 : locked int := 1;
    ll_number_2 : locked := 1;
}
```

## Routine specification
Routines are specified with an access modifier 'internal' or 'external', a name, 0 or more parameters and a body.

The parameters must be typed, and can additonally include the 'locked' keyword, to declare that a parameter will not be modified by the routine.