# Top level
At the top level global variables, structs, routines and file interactions exist. Everything defined at this level is accessible from anywhere else in the file.

___
## Global Declaration
This works exactly like usual [declarations](StatementsAndDeclarations.md#declarations), except that type inference is not currently available.
___
## File merging
**Syntax:** merge _file_path_ ;
<br>
**Explanation:** Merges the referenced file in the compilation, making everything in the file available. This can be compared to copy/pasteing the contents of one file into another. The file path name must end with .ix, or it will not be parsed.
<br>
**Example**
```
merge ./library/collections/list.ix;
```
___
## File insertion
**Syntax:** insert _file_path_ ;
<br>
**Explanation:** NOT IMPLEMENTED. Similar to merging, except that the internals of the inserted file will be hidden.
<br>
**Example**
```
insert ./library/collections/list.ix;
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
<br> entry _routine_name_ ( _simple_parameters_ ) [_block_](StatementsAndDeclarations.md#block)
<br>
**Explaination:** Type variables are a single capital letter, and parameters consist of a variable name and its type. Internal routines are only available in the file in which it is defined, or from a merged file. External routines are available in the file in which it is declared, from merged or inserted files. Entry routines are available everywhere, including as entrypoints, but they cannot be generic or have structs or arrays as parameters.
<br>
**Examples**
```
entry main(x: int) {
    s ::= new stack(1, null);
    push(2, s);
    push(3, s);
}

external load_4(to: int) {
    to := 4;
}

internal push<T>(element: T, stck: stack<T>) {
    stck := {element, stck};
}
```
___