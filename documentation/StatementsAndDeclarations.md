# Declarations
To use some variable name, it must first be declared. This can be done withing a routine to define a variable locally, or outside of routines to define a variable globally. <br>
When declaring a local variable with an assignment of some [expression](Expressions.md), the type can be, partially or entirely, left out and the compiler will attempt [infer](TypesAndProtection.md#Inference) the type from the expression.
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
In Seplin a statement has no inherent value, but still does useful things like state management, printing and control flow. 
___
## If
**Syntax:** 
<br> if ( _bool_expression_ ) _statement_ 
<br> if ( _bool_expression_ ) _statement_ else _statement_
<br> if ( _expression_ ) { is ( _constant_ ) _statement_ ... }
<br> if ( _expression_ ) { is ( _constant_ ) _statement_ ... } else _statement_
<br>
**Explaination:** Conditionally execute some code.
<br>
**Examples:**
```
if (x < 10) x := 10;
if (x < 10) x +:= 1; else x -:= 1; 
if (i) {
    is (1) print "one\n";
    is (2) print "two\n";
    is (3) print "three\n";
} else print "something\n";
```
___
## While
**Syntax:** while ( _bool_expression_ ) _statement_
<br>
**Explaination:** Keep executing some code, as long as some condition holds.
<br>
**Examples:**
```
while (x < 10) x +:= 1;
```
___
## For
**Syntax:** for ( _declaration_ ; _bool_expression_ ; _non_controlflow_statement_ ) _statement_
<br>
**Explaination:** Syntactic sugar for a while-loop with the usual control code as part of the statement.
<br>
**Examples:**
```
for (x ::= 0; x < 10; x +:= 1) print x, '\n';
```
___
## Repeat
**Syntax:** 
<br>
repeat ( _int_expression_ ) _statement_
<br>
repeat _statement_
<br>
**Explaination:** Executing some code, some fixed amount of times, or if no integer is provided, execute the code infinitly.
<br>
**Examples:**
```
i ::= 0;
repeat (10) { 
    print i, '\n; 
    i +:= 1;
}

repeat {
    print i, '\n; 
}
```
___
## Block
**Syntax:** { _statement_or_declaration_list_ }
<br>
**Explaination:** Execute the contained code sequentially. Variables declared within a block, disappear when exiting the block.
<br>
**Examples:**
```
{
    x ::= 12;
    print x;
} 
```
___
## Assign
**Syntax:** _variable_name_ := [_expression_](Expressions.md) ;
<br>
**Explaination:** Assign a new value to a variable. If the assignment operator is prefixed with an operator, it behaves differently.
<br>
**Examples:**
```
x := x + 1;
x +:= 1;
b := b && a;
```
___
## Call
**Syntax:**
<br>
 _routine_name_ < _type_arguments_ > ( _arguments_ ) ;
<br>
 _context_alias_ # _routine_name_ < _type_arguments_ > ( _arguments_ ) ;
<br>
**Explaination:** Call a routine. In some cases the type arguments can be left out, and the compiler will [infer](TypesAndProtection.md#Inference) the types from the arguments. Types cannot be infered from _null_. If a context alias is provided, the routine lookup will happen in the aliased context, otherwise the lookup will happen in the local context.
<br>
**Examples:**
```
s: int;
sum([1,2,3,4,5], s);

lst: list<int>;
num: int;
push<int>(1, lst);
push(2, lst);
funcs#pop(lst, num);
```
___
## Stop
**Syntax:** stop ;
<br>
**Explaination:** Stop executing the current routine, and return to the previous routine. If there is no previous routine, the program halts. 
<br>
**Examples:**
```
internal decrement_to_zero ::= (x: int) {
    if (x = 0) stop;
    x -:= 1;
}
```
___
## Halt
**Syntax:** 
<br>
halt ;
<br>
halt _expressions_ ;
<br>
**Explaination:** Prints any given expressions, like the Print statement, then halts the program.
<br>
**Examples:**
```
internal access ::= <T>(a: T[], i: int; out: T) {
    if (i >= |a|) halt "Index out of bounds\n";
    out := a[i];
}

entry main ::= () {
    print "Hello world!\n";
    halt;
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
```
arr ::= [1,2,3,4,5,6];
sum ::= 0;
for(i ::= 0; i < |arr|; i +:= 1) {
    if (arr[i] < 3) continue;
    sum +:= arr[i];
}
```
___
## Print
**Syntax:** print _expressions_ ;
<br>
**Explaination:** Print out the value of the types bool, int, char, char[]. For structs and other arrays, the printed value is their memory address.
<br>
**Examples:**
```
print true;
print 'H','e','l','l','o',' ','w','o','r','l','d','!','\n';
print "Hello world!\n";
```
___

# Assignment prefixes
| Assignment Prefix | Type | Explaination |
| --- | --- | --- | 
| + | int | Add to the current value |
| - | int | Subtract from the current value |
| * | int | Multiply the current value |
| ! | bool | Negate the value before assignment |
