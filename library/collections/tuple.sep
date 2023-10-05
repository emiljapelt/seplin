
reference ./stack.ix as stack;

struct tuple<T,U>(fst: T, snd: U);

external sum(t: const tuple<int,int>, out: int) {
    out := t.fst + t.snd;
}

external product(t: const tuple<int,int>, out: int) {
    out := t.fst * t.snd;
}

external zip<T,U>(stack1: stack<T>, stack2: stack<U>, out: stack<tuple<T,U>>) {
    s1 ::= stack1;
    s2 ::= stack2;
    out := null;
    until((s1 = null) || (s2 = null)) {
        out :=  {{s1.top, s2.top}, out};
        s1 := s1.rest;
        s2 := s2.rest;
    }
}