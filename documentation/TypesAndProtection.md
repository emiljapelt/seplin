# Types
| Type | Size | Default value | Classification |
| --- | --- | --- | --- |
| int | 8B | 0 | simple |
| char | 1B | '0' | simple |
| bool | 1B | false | simple |
| array[size]  | size * 8B  | null | structure |
| struct | fields * 8B | null | structure |
| routine | 8B | null | routine |

# Protection
Sometimes it might be nice to protect data from modification. In Seplin this is made possible by the usage of protection level modifiers on variables, which restrict how the variable can be used. There exists 3 levels of protection:
| Level | Syntax | Explaination |
| --- | --- | --- |
| Open | N/A | The default level, which is applied if no other level is specified. Provides no protection whatsoever. |
| Stable | stable | Provides protection from modification to the underlying data, e.g. a reference to a list can be made to reference another list, but the list itself cannot be modified. |
| Constant | const | Provides total protection from modification, e.g. both the reference to the list and the list itself cannot be modified. |

Here comes some example use cases:
1. Having a constant datastructure, while still being able to traverse it with a stable variable.
2. Declaring guaranties for a routine, for example promising that summing a ```list<int>``` will not modify said list.

# Inference
To avoid unnecessary verbosity you can leave out types or parts of types, whenever the complete type can be infered from the context, e.g. routine arguments, declarations with assignments. Here comes some examples with explainations:
```
struct tuple<T,U>(fst: T, snd: U);
struct list<T>(head: T, tail: list<T>);

entry main() {
  t0 :tuple<int,char>:= {1,'a'};
  t1 :tuple:= {1,'a'};
  t2 :tuple<int,_>:= {null, 'a'};
  t3 :tuple<list,list>:= {
    {1,{2,null},
    {'a',{'b',null}
  };
}
```
- 't0' is a explicitly declared with a fully defined type, which is a bit verbose.
- 't1' is declared as a partially defined tuple, and the generic types are infered from the assignment.
- 't2' is also declared as a partially defined tuple, but because everything can be 'null', no type can be infered, and so an explicit type is required.
- 't3' is also declared as a partially defined tuple, but here the generics are incomplete. The inference system can however handle this, and infer the types of the lists.

