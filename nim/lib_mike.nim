#[
///usr/bin/env nim r "$0" "$@" ; exit $?
]#

import mike
import std/strformat
import std/json
import std/sequtils

func fmtMethodAndPath(ctx: Context): string =
  fmt"[{ctx.request.httpMethod.get()}] {ctx.request.path.get()}"

"/" -> [get, post] do:
  ctx.send "Hello mike"

"/" -> before[get, post] do:
  echo "Before..." & fmtMethodAndPath(ctx)

"/" -> after[get, post] do:
  echo "After..." & fmtMethodAndPath(ctx)

"/error" -> [get, post] do:
  raise newException(ValueError, "Error: " & fmtMethodAndPath(ctx))

ValueError -> thrown do:
  ctx.send(code = Http500, body = "Error: " & fmtMethodAndPath(ctx))

####################
#                  #
#  Book Resources  #
#                  #
####################

type Book = object
  id: int
  name: string

var books =
  @[
    Book(id: 1, name: "Mastering Nim"),
    Book(id: 2, name: "Nim in action"),
    Book(id: 3, name: "Nim programming book"),
  ]

"/books" -> get do:
  {.gcsafe.}:
    ctx.json = books # encode json and send

"/books/:id" -> get(id: int) do:
  {.gcsafe.}:
    ctx.json = books.filterIt(it.id == id)

"/books" -> post do:
  {.gcsafe.}:
    books.add ctx.json(Book) # decode json body
    ctx.json = books[^1]

"/books/:id" -> delete(id: int) do:
  {.gcsafe.}:
    books = books.filterIt(it.id != id)
    ctx.json = books

"/books/:id" -> put(id: int) do:
  {.gcsafe.}:
    for book in books.mitems:
      if book.id == id:
        book.name = ctx.json()["name"].getStr() # read json
        break
    ctx.json = books

run(port = 8000, threads = 8)
