# Top level
At the top level global variables, structs and file interactions exist. Everything defined at this level is accessible from anywhere else in the file.

___
## Variable Declaration
This declares a variable accessible to the context, in which it is declared. This is also how routines are declared, by assigning an anonymous routine to a variable. 

Syntactically these are exactly like usual [declarations](StatementsAndDeclarations.md#declarations), except that type inference is not currently available, and they must start with an access modifier, which is either 'internal', 'external' or 'entry'. 'entry' can only apply to routine variables.
<br>
**Example**
```
internal counter :int:= 0;

external some_routine ::= <T>(v: T) {
    ...
};

entry main ::= (i: int) {
    ...
};
```
Internal variables are only available in the context in which it is defined. External variables are available in the context in which it is declared, and to other contexts. Entry variables are like external variables, and additionally they can be used as entrypoints to the file in which they are declared, but they cannot be generic or have structs, routines or arrays as parameters.

By default variable are open to modification, including routine variables. To protect data from modification [variable modifiers](TypesAndProtection.md#protection) can be applied.
___
## File referencing
**Syntax:** reference _file_path_ as _context_alias_ ;
<br>
**Explanation:** Create a reference to another file context, under the given alias. This will make struct definitions and entry/external routines from the referenced context available. Entry routines from the referenced context, will not be added to the list of entry points, but will be usable as external routines.
<br>
**Example**
```
reference ./library/collections/list.sep as list;
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