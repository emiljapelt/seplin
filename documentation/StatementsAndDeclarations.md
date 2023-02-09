# Declarations
To use some variable name, it must first be declared. This can be done withing a routine to define a variable locally, or outside of routines to define a variable globally. Variables declared without an assignment, will initially be _null_. <br>
When declaring a local variable with an assignment of some [expression](Expressions.md), unless that expression is a [struct literal](Expressions.md#structliteral), the type can be left out and the compiler will infer the type.
<br>
**Syntax:** <br>_variable_name_ : _type_ ;  <br>  _variable_name_ : _type_ := _expression_ ;
<br>
**Examples:**
```
x: int;
y :int:= 2;
z ::= 3;
t :tuple<int,int>:= {1,2};
```

# Statements
In INEX a statement has no inherent value, but still does useful things like state management, printing and control flow. 
___
## If
**Syntax:** <br> if ( _bool_expression_ ) _statement_ <br> if ( _bool_expression_ ) _statement_ else _statement_
<br>
**Explaination:** Conditionally execute some code
<br>
**Examples:**
```
if (x < 10) x := 10;
if (x < 10) x +:= 1; else x -:= 1; 
```
___
## While
**Syntax:** while ( _bool_expression_ ) _statement_
<br>
**Explaination:** Keep executing some code, as long as some condition holds
<br>
**Examples:**
```
while (x < 10) x +:= 1;
```
___
## Block
**Syntax:** { _statement_or_declaration_list_ }
<br>
**Explaination:** Execute the contained code sequentially. Variables declared within a block, disapper when exiting the block.
<br>
**Examples:**
```
while (x < 10) {
    x +:= 1;
    print x;
} 
```
___
## Assign
**Syntax:** _variable_name_ := [_expression_](Expressions.md) ;
<br>
**Explaination:** Assign a new value to a variable. If the assignment operator is prefixed with an operator is behaves differently.
<br>
**Examples:**
```
x := x + 1;
x +:= 1;
b := b && a;
```
___
## Call
**Syntax:** _routine_name_ < _type_arguments_ > ( _arguments_ ) ;
<br>
**Explaination:** Call a routine. In some cases the type arguments can be left out, and the compiler will infer the types from the arguments. Types cannot be infered from _null_ and [struct literals](Expressions.md#structliteral).
<br>
**Examples:**
```
s: int;
sum([1,2,3,4,5], s);

lst: list<int>;
push<int>(1, lst);
push(2, lst);
```
___
## Stop
**Syntax:** stop ;
<br>
**Explaination:** Stop executing the current routine, and return to the previose routine. If there is no previous routine, the program halts. 
<br>
**Examples:**
```
internal decrement_to_zero(x: int) {
    if (x = 0) stop;
    x -:= 1;
}
```
___
## Halt
**Syntax:** halt ;
<br>
**Explaination:** Halt the machine.
<br>
**Examples:**
```
internal access<T>(a: T[], i: int; out: T) {
    if (i >= |a|) halt;
    out := a[i];
}
```
___
## Break
**Syntax:** break ;
<br>
**Explaination:** Break out of the current loop.
<br>
**Examples:** 
```
i ::= 1;
while (true) {
    if (i > 10) break;
    else i +:= 1;
}
```
___
## Continue
**Syntax:** continue ;
<br>
**Explaination:** Stop executing the current loop iteration, and go to the conditional check.
<br>
**Examples:**
___
## Print
**Syntax:** print _expressions_ ;
<br>
**Explaination:** Print out the values of some expressions. For structs and arrays, the printed value is their memory address.
<br>
**Examples:**
```
print true;
print 'H','e','l','l','o',' ','w','o','r','l','d','!','\n';
```
___

# Assignment prefixes
| Assignment Prefix | Type | Explaination |
| --- | --- | --- | 
| + | int | Add to the current value |
| - | int | Subtract from the current value |
| * | int | Multiply the current value |
| ! | bool | Negate the value before assignment |