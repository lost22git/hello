#!/usr/bin/env -S roc dev
app [main] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.15.0/SlwdbJ-3GR7uBWQo6zlmYWNYOxnvo8r6YABXD-45UOw.tar.br" }

import pf.Stdout

User a : {
    name : Name,
    age : U8,
}a

Name : {
    first : Str,
    last : Str,
}

greetPartial : { name : { first : Str, last : Str } }* -> Task {} _
greetPartial = \user ->
    Stdout.line! "user.name: $(user.name.first) $(user.name.last)"

greetExtend : User * -> Task {} _
greetExtend = \user ->
    greetPartial! user
    Stdout.line! "user.age: $(Num.toStr user.age)"

greetExact : User {} -> Task {} _
greetExact = \user ->
    greetExtend! user

main =
    partialUser = { name: { first: "foo", last: "bar" } }
    exactUser = { name: { first: "foo", last: "bar" }, age: 10 }
    extendUser = { name: { first: "foo", last: "bar" }, age: 10, gender: Male }
    greetPartial! partialUser
    greetPartial! exactUser
    greetPartial! extendUser
    Stdout.line! (Str.repeat "-" 66)
    greetExtend! exactUser
    greetExtend! extendUser
    Stdout.line! (Str.repeat "-" 66)
    greetExact! exactUser
