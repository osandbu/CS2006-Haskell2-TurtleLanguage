main(){
    JumpTo -40 100
    magic(4)
}
magic(n) {
    if(n>0){
        Color (0, 255, n*63.75)
        repeat 270 {
            Fd 1
            Lt 1
        }
        Fd 150
        magic(n-1)
    }
}
