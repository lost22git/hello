module [
    divrem,
    divmod,
]

divrem : Int a, Int a -> { quo : Int a, rem : Int a }
divrem = \toDivide, divivor -> { quo: toDivide // divivor, rem: toDivide % divivor }

expect divrem 11 3 == { quo: 3, rem: 2 }
expect divrem 11 -3 == { quo: -3, rem: 2 }
expect divrem -11 -3 == { quo: 3, rem: -2 }
expect divrem -11 3 == { quo: -3, rem: -2 }

divmod : Int a, Int a -> { quo : Int a, mod : Int a }
divmod = \toDivide, divivor ->
    { quo, rem } = divrem toDivide divivor
    if Num.compare rem 0 == EQ then
        { quo: quo, mod: 0 }
    else if quo >= 0 then
        { quo: quo, mod: rem }
    else
        { quo: quo - 1, mod: (toDivide - (quo - 1) * divivor) }

expect divmod 11 3 == { quo: 3, mod: 2 }
expect divmod 11 -3 == { quo: -4, mod: -1 }
expect divmod -11 -3 == { quo: 3, mod: -2 }
expect divmod -11 3 == { quo: -4, mod: 1 }
