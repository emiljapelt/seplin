# Top level
At the top level global variables, structs, routines and file interactions exist. Everything defined at this level is accessible from anywhere else in the file.

___
## Global Declaration
This works exactly like usual [declarations](StatementsAndDeclarations.md#declarations), except that type inference is not currently available.
___
## File referencing
**Syntax:** reference _file_path_ as _context_alias_ ;
<br>
**Explanation:** Create a reference to another .ix-file's context, under the given alias. This will make struct definitions and entry/external routines from the context available. Entry routines from the referenced context, will not be added to the list of entry points, but will be usable as external routines.
<br>
**Example**
```
reference ./library/collections/list.ix as list;
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