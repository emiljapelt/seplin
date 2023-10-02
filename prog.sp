
struct list<T>(head: T, tail: list<T>);
struct tuple<T>(fst: T, snd: T);


entry main(steps: int) {

    l_int :list:= {1,{2,{3,{4,null}}}};
    l_bool : list<bool>;

    translate<int,bool>(l_int, lt_3, l_bool);
    print l_bool.head, l_bool.tail.head, l_bool.tail.tail.head, l_bool.tail.tail.tail.head, '\n';


    t :tuple:= {0,1};
    fs: list<(tuple<int>)>;

    init<(tuple<int>)>(fs, steps, fib_step);
    collapse<tuple<int>>(fs, t);
    print steps, ' ', t.snd, '\n';
}


internal fib_step(t: tuple<int>) {
    t := { t.snd, t.fst + t.snd };
}

internal init<T>(l: list<T>, size: const int, default: T) {
    l := null;
    repeat(size) {
        l := {$default, l};
    }
}

internal collapse<T>(fs: const list<(T)>, acc: T) {
    handle :stable:= fs;
    until(handle = null) {
        handle.head(acc);
        handle := handle.tail;
    }
}

external translate<T,U>(in_list: list<T>, f:(T,U), out_list: list<U>) {
    if (in_list = null) stop;
    
    temp_t ::= $in_list.head;
    temp_u : U;
    f(temp_t, temp_u);

    temp_tail :list<U>:= null;
    translate<T,U>(in_list.tail, f, temp_tail);

    out_list := {temp_u, temp_tail};
}

internal lt_3(i: int, b: bool) {
    temp_b : bool;
    if (i < 3) temp_b := true;
    else temp_b := false;
    b := temp_b;
}