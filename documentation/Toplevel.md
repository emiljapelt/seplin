# Top level
At the top level global variables, structs, routines and file inclusions exist. Everything at this level is accessible from anywhere.

___
## Global Declaration
This works exactly like usual [declarations](StatementsAndDeclarations.md#declarations), except that type inference is not currently available.
___
## File Inclusion
**Syntax:** include _file_path_ ;
<br>
**Explanation:** Includes the referenced file in the compilation, making everything in the file available. The file name must end with .ix, or it will not be parsed.
<br>
**Example**
```
include ./library/collections/list.ix;
```
___
## Structs
**Syntax:** struct _struct_name_ < _type_variables_ > ( _fields_ ) ;
<br>
**Explaination:** Type variables are a single capital letter, and fields consist of a variable name and its type.
<br>
**Examples:**
```
struct int_tuple(fst: int, snd: int);
struct stack<T>(top: T, rest: stack<T>);
```
___
## Routines
**Syntax:** <br> internal _routine_name_ < _type_variables_ > ( _parameters_ ) [_block_](StatementsAndDeclarations.md#block)
<br> external _routine_name_ < _type_variables_ > ( _parameters_ ) [_block_](StatementsAndDeclarations.md#block)
<br>
**Explaination:** Type variables are a single capital letter, and parameters consist of a variable name and its type.
<br>
**Examples**
```
external main(x: int) {
    s ::= new stack(1, null);
    push(2, s);
    push(3, s);
}

internal push<T>(element: T, stck: stack<T>) {
    stck := {element, stck};
}
```
___