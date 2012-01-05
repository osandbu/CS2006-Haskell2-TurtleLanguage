test (x)
{
    if (x > 1) {
        test(x / 3)
        Lt 60
        test(x / 3)
        Rt 120
        test(x / 3)
        Lt 60
        test(x / 3)
    } else {
        Fd 1
    }
}
main() {
    Color(50,122,21)
    JumpTo 0 120
    repeat 3 {
        Rt 120
        test(100)
    }
}
