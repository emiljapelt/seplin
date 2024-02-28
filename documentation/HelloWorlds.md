
# How to write "Hello World!" in Seplin

In this document at handful of ways to implement the Hello World-program, is explored and explained, in an attempt to show of some features, including syntactic sugar, of the Seplin language.

## Basic
The most basic way to print "Hello World!", would be to use the 'print' statement. 
```
entry main1 ::= (){

    print 'H','e','l','l','o',' ','W','o','r','l','d','!','\n';

};
```

## Basic - Sugared
Having to write out character literals like that is horrible, luckily Seplin knows how to print character arrays, and also includes string literals as syntactic sugar for these. So instead the implementation could be:
```
entry main2 ::= (){

    print "Hello World!\n";

};
```

## Loop
Much better! But how does that work under the layer of sugar? Well it simply loops over the length of the character array, and prints each one. Like so:
```
entry main3 ::= (){

    msg ::= "Hello World!\n";
    for(i ::= 0; i < |msg|; i +:= 1) print msg[i];

};
```
## Routine
If the default printing behavior is missing something for a use case, one can simply implement a routine, to gain the extra behavior. For example, if one would like to always print a linefeed after some message:
```
internal out_ln ::= (str: const char[]){
    for(i ::= 0; i < |str|; i +:= 1) print str[i];
    print '\n';
};

entry main4 ::= (){

    out_ln("Hello World!");

};
```

## String library
Nice! But defining such a routine in each source file is annoying, so extract it to a library and reference that.
```
reference ./string.sep as string;

entry main5 ::= (){

    string#out_ln("Hello World!");

};
```

## Higher-order routine
Finally we can utilize the fact that in Seplin, routines are just values which can be given as arguments, reassigned, etc., to write a generalized array iterator which applies a routine to each element:  
```
internal fout ::= (c: char){
    print c;
};

internal iter ::= <T>(a: T[], f: (T)){
    for(i ::= 0; i < |a|; i +:= 1) f(a[i]);
};

entry main6 ::= (){

    iter("Hello World!\n", fout);

};
```
