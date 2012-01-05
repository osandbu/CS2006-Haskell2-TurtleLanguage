circle(lt){
    repeat 360 { Fd 2
        if (lt==1) { Lt 1 }
        else       { Rt 1 }
    }
}
line(length) {
    lineR(length, length)
}
lineR(n, length){
    if (n > 0) {
        Color (255, n*1.7, 0)
        Fd 1
        line(n-1, length)
    }
}
main() {
    JumpTo 75 0
    Color YELLOW
    circle(0)
    Lt90
    line(150)
    Rt90
    ColorRED
    circle(1)
}
