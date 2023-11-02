
# How to write "Hello World!" in Seplin

## Basic
The most basic way to print "Hello World!", would be to use the 'print' statement. 
```
entry main1() {

    print 'H','e','l','l','o',' ','W','o','r','l','d','!','\n';

}
```

## Loop
Writing out each character as a literal, is however very tedious. Instead one could write the message out as a string (syntacic sugar for a character array), and use a loop for printing.
```
entry main2() {

    msg ::= "Hello World!";
    for(i ::= 0; i < |msg|; i +:= 1) print msg[i];
    print '\n';

}
```
## Routine
Better, but still a bit tedious having to use multiple lines, and write out a loop each time one wants to print a string. This can be fixed by simply extracting to a routine.
```
internal out_ln(str: const char[]) {
    for(i ::= 0; i < |str|; i +:= 1) print str[i];
    print '\n';
}

entry main3() {

    out_ln("Hello World!");

}
```

## String library
Nice! But defining such a routine in each source file is annoying, so extract it to a library and reference that.
```
reference ./string.sep as string;

entry main4() {

    string#out_ln("Hello World!");

}
```

## Higher-order routine
Using a library is likely the nicest way to do this, but we can still make it weirder by using higher-order routines (Which could also be from a library). 
```
internal fout(c: char) {
    print c;
}

internal iter<T>(a: T[], f: (T)) {
    for(i ::= 0; i < |a|; i +:= 1) f(a[i]);
}

entry main5() {

    iter("Hello World!", fout);
    print '\n';

}
```