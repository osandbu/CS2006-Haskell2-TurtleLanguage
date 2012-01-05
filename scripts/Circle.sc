blueCircle(x, n){
    Color (0, n, n)
    Fd x
    Rt 1
    if (n < 360) { blueCircle(x,n+1) }
}
main(){
    PenUp
    MoveTo -100 0
    PenDown
    blueCircle(2,1)
}
