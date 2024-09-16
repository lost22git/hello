#[
///usr/bin/env nim r "$0" "$@" ; exit $?
]#

import std/[strformat]
import std/[macros, genasts]

template `===`(title: static[string]) =
  block:
    let t {.inject.} = title
    echo fmt"------{t:-<30}"

## Ast

template expectKinds(range, i, body: untyped) =
  expectKind(range, nnkIdent)
  expectKind(i, nnkIdent)
  expectKind(body, nnkStmtList)

macro testGenAst(range, i, body: untyped): untyped =
  expectKinds(range, i, body)
  result = genAst(range, i, body):
    for i in range:
      body

macro testQuote(range, i, body: untyped): untyped =
  expectKinds(range, i, body)
  result = quote:
    for `i` in `range`:
      `body`

template testTemplate(range, i, body: untyped) =
  for i in range:
    body

# dumpAstGen:
#   for i in range:
#     body
macro testAstConstruct(range, i, body: untyped): untyped =
  result = nnkStmtList.newTree(nnkForStmt.newTree(i, range, body))

var items = 5 .. 8

==="test gen ast"
testGenAst items, item:
  echo fmt"{item = }"

==="test quote"
testQuote items, item:
  echo fmt"{item = }"

==="test template"
testTemplate items, item:
  echo fmt"{item = }"

==="test ast construct"
testAstConstruct items, item:
  echo fmt"{item = }"

==="宏与编译提示"
import std/[times, monotimes, os, typedthreads]
proc logEnterProc(procName: string) {.inline.} =
  echo fmt"""{now().utc} ({getThreadId():<6}) | ENTER procName: "{procName}""""

proc logExitProc(procName: string, st: MonoTime, e: ref Exception) {.inline.} =
  if not isNil(e):
    echo fmt"""{now().utc} ({getThreadId():<6}) | EXIT procName: "{procName}", elap: "{(getMonoTime()-st).inMilliseconds}ms", error: "{e.msg}""""
  else:
    echo fmt"""{now().utc} ({getThreadId():<6}) | EXIT procName: "{procName}", elap: "{(getMonoTime()-st).inMilliseconds}ms""""

proc logExitProc[T](procName: string, st: MonoTime, r: T, e: ref Exception) {.inline.} =
  if not isNil(e):
    echo fmt"""{now().utc} ({getThreadId():<6}) | EXIT procName: "{procName}", elap: "{(getMonoTime()-st).inMilliseconds}ms", error: "{e.msg}""""
  else:
    echo fmt"""{now().utc} ({getThreadId():<6}) | EXIT procName: "{procName}", elap: "{(getMonoTime()-st).inMilliseconds}ms", result: "{r.repr}""""

# dumpAstGen:
#   proc test(): int =
#     let st = getMonoTime()
#     logEnterProc("test")
#     try:
#       result = 1
#     finally:
#       logExitProc[int]("test", st, result, getCurrentException())

macro tracing(p: untyped): untyped =
  expectKind(p, nnkProcDef)
  let
    procName = $(p.name) # proc name
    resultTypeNode = p.params[0]

  let newBody = nnkStmtList.newTree(
    nnkLetSection.newTree(
      nnkIdentDefs.newTree(
        newIdentNode("_st"),
        newEmptyNode(),
        nnkCall.newTree(newIdentNode("getMonoTime")),
      )
    ),
    nnkCall.newTree(newIdentNode("logEnterProc"), newLit(procName)),
    nnkTryStmt.newTree(
      p.body, # proc old body
      nnkFinally.newTree(
        nnkStmtList.newTree(
          if resultTypeNode.kind == nnkEmpty:
            # the result type of the proc is not exist
            # logExitProc(procName,st,error)
            nnkCall.newTree(
              newIdentNode("logExitProc"),
              newLit(procName),
              newIdentNode("_st"),
              nnkCall.newTree(newIdentNode("getCurrentException")),
            )
          else:
            # the result type of the proc is T
            # logExitProc[T](procName,st,result,error)
            nnkCall.newTree(
              nnkBracketExpr.newTree(newIdentNode("logExitProc"), resultTypeNode),
              newLit(procName),
              newIdentNode("_st"),
              newIdentNode("result"),
              nnkCall.newTree(newIdentNode("getCurrentException")),
            )
        )
      ),
    ),
  )
  p.body = newBody
  result = p

proc testNoResultType(duration: Duration) {.tracing.} =
  sleep duration.inMilliseconds()

proc testResultType(x, y: int): int {.tracing.} =
  sleep 1000
  result = x + y

proc testRaiseError(x, y: int): int {.tracing.} =
  sleep 1000
  raise newException(ValueError, "some error raised")

testNoResultType initDuration(seconds = 1)

discard testResultType(44, 44)

try:
  discard testRaiseError(44, 44)
except:
  discard

==="字段与编译提示"
import std/macros

template opt() {.pragma.}
template opt2() {.pragma.}

type A = object
  name {.opt, opt2.}: string
  age: int

var a = A(name: "foo", age: 10)

# expect like below:
#
# case k
# of "name":
#   a.name = v
# else:
#   discard
# 
macro parse(a: typed, k, v: untyped): untyped =
  # case k
  result = nnkCaseStmt.newTree(k)
  # of branches
  let typeImpl = a.getTypeInst().getImpl()
  # echo typeImpl.treeRepr() # 打印 type desc ast
  for fieldDef in typeImpl[2][2]:
    if fieldDef[0].kind == nnkPragmaExpr:
      for pragma in fieldDef[0][1]:
        if pragma.eqIdent("opt"):
          let fieldNameIdent = fieldDef[0][0]
          result.add nnkOfBranch.newTree(
            newLit(fieldNameIdent.strVal()),
            nnkStmtList.newTree(
              nnkAsgn.newTree(nnkDotExpr.newTree(a, fieldNameIdent), v)
            ),
          )
  # else: discard
  result.add nnkElse.newTree(
    nnkStmtList.newTree(nnkDiscardStmt.newTree(newEmptyNode()))
  )

let k = "name"
let v = "🤔"
parse a, k, v
echo a

==="parseExpr"
import std/[sequtils, strutils]

macro filter(expr: static string, p: untyped): untyped =
  expectKind(p, nnkProcDef)
  let returnType = p.params[0]
  expectIdent(returnType, "bool")
  let firstParamDef = p.params[1]
  expectKind(firstParamDef, nnkIdentDefs)
  expectKind(firstParamDef[1], nnkBracketExpr)
  expectIdent(firstParamDef[1][0], "seq")
  expectIdent(firstParamDef[1][1], "string")
  let firstParamName = firstParamDef[0]

  let filterExpr = parseExpr(expr)

  let newBody = nnkStmtList.newTree(
    nnkAsgn.newTree(
      newIdentNode("result"),
      nnkCall.newTree(
        nnkDotExpr.newTree(firstParamName, newIdentNode("anyIt")), filterExpr
      ),
    )
  )
  p.body = newBody
  result = p

proc findAny(v: seq[string]): bool {.filter: """it == "foo" or it.startswith("f")""".}

doAssert not @["hello", "kooo"].findAny()
doAssert @["hello", "fooo"].findAny()
doAssert @["hello", "foo"].findAny()

==="指定函数名调用相应函数"
proc print(x: string) =
  echo x

template haha(fn: static string) {.pragma.}

type Foo = object
  bar {.haha: "print".}: string

macro callFn(f: static string, vv: untyped): untyped =
  nnkStmtList.newTree(nnkCommand.newTree(newIdentNode(f), vv))

proc vv(foo: Foo) =
  for fk, fv in foo.fieldPairs():
    when fv.hasCustomPragma(haha):
      const ff = fv.getCustomPragmaVal(haha).fn
      callFn(ff, fv)

Foo(bar: "bar").vv()
