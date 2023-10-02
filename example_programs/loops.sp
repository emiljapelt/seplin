
external run(loops: int) {
    until(loops = 0) toward_zero(loops);
}

internal toward_zero(x: int) {
    print x, '\n';
    if (x > 0) x -:= 1;
    else if (x < 0) x +:= 1;
    else stop;
}