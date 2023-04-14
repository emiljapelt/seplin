# INEX, a nonfunctional programming language

## Features
| Feature | Explaination |
| --- | --- |
| Side-effect oriented programming | Instead of functions/methods which can return some data, INEX uses routines, which cannot return anything. However, all data is stored on the heap, and by default arguments are passed by reference, so changes made in a routine are persisted. |
| Full null'ability | Every variable can be null.  |
| True global top-level | At the top-level of a context, order does not matter. Every routine can use all global variables, all structs and all routines. The compiler will resolve an ordering of global variables, such that circular dependencies will be detected. |
| Multi entry point | Routines are marked either 'internal', 'external' or 'entry'. When running the program, any of the entry routines can be used as the entry point. |
| Generic types | Both structs and routines can be generic. |
| Type inference | Both generic types, and the type of non-global variable declarations, can be infered in most cases.  |


## Special words
| Word | Explaination |
| --- | --- |
| Context | Denotes everything that is available directly in a file. This includes structs, global variables, routines and references to other contexts.


## Documentation Index
### [Types](./documentation/Types.md)
### [Expressions](./documentation/Expressions.md)
### [Statements and declarations](./documentation/StatementsAndDeclarations.md)
### [Top level](./documentation/Toplevel.md)