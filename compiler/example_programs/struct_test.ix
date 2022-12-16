funny1 : locked int := 69;
one : locked int := 1;
addd : locked int := funny1 + one;

global_tuple : tuple := new tuple(null,null);

struct tuple (fst: int, snd: int);
struct person (age: int, favoriteNumbers: tuple);

external str() {
    x : tuple;
    a ::= 1;

    x := global_tuple;
    print (x.fst = null), '\n';
    x.fst := a;
    print (x.fst = null), '\n';
    x.fst := null;
    print (x.fst = null), '\n';
}

external arr() {
    x ::= new int[3];
    print |x|, '\n';
    init_array(x);
    a: int := 4;
    b: int := 5;
    x[0] := $a;
    x[1] := 3;
    print x[0], '\n';
    print x[1], '\n';
    print |x|, '\n';
}

internal init_array(arr: int[]) {
    print |arr|, '\n';
    for(i: int := 0; i < |arr|; i +:= 1) {
        e: int := 0;
        arr[i] := e;
    }
}

external pers() {
    emil: var := new person(23, new tuple(8, 16));
    
    print_person(emil);
    print emil.favoriteNumbers.fst, '\n';
}

internal print_person(p: locked person) {
    print p.age, '\n';
    print p.favoriteNumbers.fst, '\n';
    print p.favoriteNumbers.snd, '\n';
}

external strarr() {
    people : var := new person[2];
    people[0] := new person(16, new tuple(0,0));
    people[1] := new person(17, new tuple(0,0));

    total: int;
    total_age(people, total);
    print total, '\n';
}

internal total_age(people: locked person[], sum: int) {
    sum := 0;
    for(i: int := 0; i < |people|; i +:= 1) {
        sum +:= people[i].age;
    }
}