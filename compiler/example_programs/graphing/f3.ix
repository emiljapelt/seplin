
struct list<T>(head: T, tail: list<T>);

external prepend<T>(element: T, target: list<T>) {
    target := { element, target };
}

meme: int;

external sum(l: list<int>, out: int) {
    out := 0;
    handle ::= $l;
    while(handle != null) {
        out +:= handle.head;
        handle := handle.tail;
    }
}

external print_list<T>(target: list<T>) {
    handle ::= $target;
    while(handle != null) {
        print handle.head, '\n';
        handle := handle.tail;
    }
}

external from_array<T>(a: T[], out: list<T>) {
    out := null;
    for(i ::= 0; i < |a|; i +:= 1) {
        out := {a[i], out};
    }
}