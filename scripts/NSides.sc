nsides(l,n,t){
    Color (255,255/n,0)
    Fd l
    Rt 360/t
    if (n > 0) { nsides(l,n-1, t) }
}
main(l,n){
    nsides(l,n,n)
}
