#!/usr/bin/env -S roc test
module [
    divrem,
    divmod,
    divfloor,
]

divrem : Int a, Int a -> { quo : Int a, rem : Int a }
divrem = \toDivide, divivor -> { quo: toDivide // divivor, rem: toDivide % divivor }

expect divrem 11 3 == { quo: 3, rem: 2 }
expect divrem -11 -3 == { quo: 3, rem: -2 }
expect divrem 11 -3 == { quo: -3, rem: 2 }
expect divrem -11 3 == { quo: -3, rem: -2 }
#
expect divrem 3 11 == { quo: 0, rem: 3 }
expect divrem -3 -11 == { quo: 0, rem: -3 }
expect divrem 3 -11 == { quo: 0, rem: 3 }
expect divrem -3 11 == { quo: 0, rem: -3 }
#
expect divrem 0 11 == { quo: 0, rem: 0 }
expect divrem 0 -11 == { quo: 0, rem: 0 }
#
expect divrem 3 3 == { quo: 1, rem: 0 }
expect divrem -3 -3 == { quo: 1, rem: 0 }
expect divrem 3 -3 == { quo: -1, rem: 0 }
expect divrem -3 3 == { quo: -1, rem: 0 }

divmod : Int a, Int a -> { quo : Int a, mod : Int a }
divmod = \toDivide, divivor ->
    { quo, rem } = divrem toDivide divivor
    if Num.compare rem 0 == EQ then
        { quo: quo, mod: 0 }
    else if quo > 0 then
        { quo: quo, mod: rem }
    else if quo < 0 then
        { quo: quo - 1, mod: (toDivide - (quo - 1) * divivor) }
    else if (toDivide >= 0 && divivor > 0) || (toDivide <= 0 && divivor < 0) then
        { quo: quo, mod: rem }
    else
        { quo: quo - 1, mod: (toDivide - (quo - 1) * divivor) }

expect divmod 11 3 == { quo: 3, mod: 2 }
expect divmod -11 -3 == { quo: 3, mod: -2 }
expect divmod 11 -3 == { quo: -4, mod: -1 }
expect divmod -11 3 == { quo: -4, mod: 1 }
#
expect divmod 3 11 == { quo: 0, mod: 3 }
expect divmod -3 -11 == { quo: 0, mod: -3 }
expect divmod 3 -11 == { quo: -1, mod: -8 }
expect divmod -3 11 == { quo: -1, mod: 8 }
#
expect divmod 0 11 == { quo: 0, mod: 0 }
expect divmod 0 -11 == { quo: 0, mod: 0 }
#
expect divmod 3 3 == { quo: 1, mod: 0 }
expect divmod -3 -3 == { quo: 1, mod: 0 }
expect divmod 3 -3 == { quo: -1, mod: 0 }
expect divmod -3 3 == { quo: -1, mod: 0 }

divfloor : Int a, Int a -> Int a
divfloor = \toDivide, divivor ->
    { quo, rem } = divrem toDivide divivor
    if Num.compare rem 0 == EQ then
        quo
    else if quo > 0 then
        quo
    else if quo < 0 then
        quo - 1
    else if (toDivide >= 0 && divivor > 0) || (toDivide <= 0 && divivor < 0) then
        quo
    else
        quo - 1
#
expect divfloor 11 3 == 3
expect divfloor -11 -3 == 3
expect divfloor -11 3 == -4
expect divfloor 11 -3 == -4
#
expect divfloor 3 11 == 0
expect divfloor -3 -11 == 0
expect divfloor 3 -11 == -1
expect divfloor -3 11 == -1
#
expect divfloor 0 11 == 0
expect divfloor 0 -11 == 0
#
expect divfloor 3 3 == 1
expect divfloor -3 -3 == 1
expect divfloor 3 -3 == -1
expect divfloor -3 3 == -1
