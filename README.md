# INEX, a nonfunctional programming language

## Features
| Feature | Explaination |
| --- | --- |
| Side-effect oriented programming | Instead of functions/methods which can return some data, INEX used routines, which cannot return anything. However, all data is stored on the heap, and by default arguments are passed by reference, so changes made in a routine are persisted. |
| Full null'ability | Every variable can be null, by not pointing to any data on the heap.  |
| Full top-level hoisting | At the top-level order does not matter, every routine can use all global variables, all structs and all routines. The compiler will also resolve an ordering of global variables, such that circular dependencies will be detected. |
| Multi entry point | Routines are marked either 'internal' or 'external'. When running the program, any of the external routines can be used as the entry point. |
| Generic types | Both structs and routines can be generic. |
| Type inference | Both generic types, and the type of non-global variable declarations, can be infered in most cases.  |

## Types
| Type | Size | Default value |
| --- | --- | --- |
| int | 8B | 0 |
| char | 1B | '0' |
| bool | 1B | false |
| []  | size * 8B  | null |
| struct | fields * 8B | null|

## Top-level 
At the top level 4 elements exists. Routines, structs, global variables and file references. The order in which these elements are written, does not matter.

### Routines
---
Syntax: internal/external _routine_name_\<_type_variables_>(_parameters_){ ... }

Examples:
```
external main() {
    print true;
}

internal array_assign<T>(a: T[], e: T) {
    a[0] := e;
}
```

### Structs
---
Syntax: struct\<_type_variables>_(_fields_);

Examples:
```
struct tuple<T,U>(fst: T, snd: U);

struct 2d_point(x: int, y: int);

struct linked_list<T>(head: T, tail: linked_list<T>);
```

### Global variables
---
Syntax:

_variable_name_: locked? _type_;

_variable_name_: locked? _type_ := _expression_;

_variable_name_ :locked?:= _expression_;

Examples:
```
call_count: int;
eight: locked int := 8;
four: locked := 4;
```

### File references
---
Syntax: include file_path;

'include' acts like a copy/paste of the contents from another source file, _f_, meaning that external routines from _f_ will be added as entrypoints, and, more generally, that everything in _f_ becomes accesible.

Examples:
```
include ../library/collections/list.ix;
```