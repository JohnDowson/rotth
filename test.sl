module foo
use bar from foo

struct [T] FooBar
    foo: T

impl FooBar[i32]
    func method self: Self => i32
        self.foo + 1

impl[T: Copy] Frobnify for FooBar[T]
    func frob self: &Self => T
        self.foo

func foo i: i64, j: u69 => i64
    static BAR: i64 = 69
    const FOO: i64 = 42
    let goo: i64 = 66
    0
    let foo = match i
        1 => 2
        3 => 4
        5 =>
            6
            7
    8
    if foo == 9
        10
    else
        11
    while false
        12
    let lam = lambda x, y, z: i32 y
    lam(6, 6, 6)
    alpha.beta.delta()
    sigma
        .zeta
        .eta(
            looongloong,
            verylong,
            a.fucking.expression,
            a.fucking
             .multiline
             .expression
        )
    let bar = Something
        a: 1
        b: "hello"
