# INEX, a nonfunctional programming language

## Features
| Feature | Explaination |
| --- | --- |
| Side-effect oriented programming | Instead of functions/methods which can return some data, INEX uses routines, which cannot return anything. However, all data is stored on the heap, and by default arguments are passed by reference, so changes made in a routine are persisted. |
| Full null'ability | Every variable can be null.  |
| Truly global top-level | At the top-level order does not matter, every routine can use all global variables, all structs and all routines. The compiler will resolve an ordering of global variables, such that circular dependencies will be detected. |
| Multi entry point | Routines are marked either 'internal' or 'external'. When running the program, any of the external routines can be used as the entry point. |
| Generic types | Both structs and routines can be generic. |
| Type inference | Both generic types, and the type of non-global variable declarations, can be infered in most cases.  |
| File reduction | Any routine which will never be reached from an external routine, is not compiled. |


## Documentation Index
### [Types](./documentation/Types.md)
### [Expressions](./documentation/Expressions.md)
### [Statements and declarations](./documentation/StatementsAndDeclarations.md)
### [Top level](./documentation/Toplevel.md)