# Seplin, a nonfunctional programming language

Seplin stands for, **s**ide-**e**ffect **p**rogramming **lin**go, and centers around not being able to return values from function. The language is statically typed, very type strict and the compiler targets a custom designed virtual machine. 

## Feature list
| Feature | Explaination |
| --- | --- |
| Side-effect oriented programming | Instead of the concepts of functions or methods, which can return some value, Seplin uses the concept of routines, which cannot return anything. However, all data is stored on the heap, and by default arguments are passed by reference, so changes made in a routine are persisted. |
| Full null'ability | Every variable can be null, yes all of them. So be very careful.  |
| True global top-level | At the top-level of a context, order does not matter. Every routine can use all global variables, all structs and all routines. The compiler will resolve an ordering of global variables, such that circular dependencies will be detected. |
| Multi entry point | Routines are marked either 'internal', 'external' or 'entry'. When running the program, any of the entry routines can be used as the entry point. |
| Generic types | Both structs and routines can be generic. |
| Flexible type inference | Both generic structures, routines, and the type of non-global variable declarations, can be infered in most cases.  |
| Higher-order routines | Routines have types and can be used as values, and can therefor be assigned to variables and datastructurs, as well as being passed to routines as arguments. |

## Infrastructure
To work with Seplin two programs are provided. Additonally, a syntax highlighter for VSCode exists, and can be found in /extenstions.
| Program | Explaination |
| --- | --- | 
| seplin | The virtual machine for Seplin. Will invoke the compiler, if given a source file. Use --help for further details. |
| seplinc | The compiler for Seplin. Takes a .sep or .sea file as input, and optionally an output directory or file. By default the output is a .sec file, which can be run by the VM. However, if the '-t' flag is set, the program is transpiled to C. |

## Special words
| Word | Explaination |
| --- | --- |
| Context | Denotes everything that is available in a file. This includes structs, global variables, routines and references to other contexts.


## Documentation Index
### [Hello Worlds](./documentation/HelloWorlds.md)
### [Top level](./documentation/Toplevel.md)
### [Statements and declarations](./documentation/StatementsAndDeclarations.md)
### [Expressions](./documentation/Expressions.md)
### [Types](./documentation/TypesAndProtection.md)
