#!/usr/bin/env cyber

use test

-- string interpolation
--
var lang = "cyber"
test.assert("hello $(lang)" == "hello cyber")


-- multilines string
--
var json = """
{
  "lang": "$(lang)"
}
"""
test.assert(json == "\n{\n  \"lang\": \"cyber\"\n}\n")

-- raw string
--
test.assert('foo' == "foo")
test.assert('foo\n' == "foo\\n")


-- index / slice
--
var doc = "👻 woo"
test.assert(doc.len() == 8)
test.assert(doc.count() == 5)
test.assert(doc[5] == `w`)
test.assert(doc.seek(2) == 5)
test.assert(doc[doc.seek(2)] == `w`)
test.assert(doc[5..] == 'woo')
test.assert(doc[5..7] == 'wo')
test.assert(doc.find('woo').? == 5)
test.assert(doc.find('👻').? == 0)
test.assert(doc.findRune(`w`).? == 5)
test.assert(doc.findRune(`👻`).? == 0)

-- split
--
var uri = 'https://github.com/foo/bar'
var segments = uri.split('/')
var expects = ['https:', '', 'github.com', 'foo', 'bar']
test.assert(segments.len() == expects.len())
test.eqList(segments ,expects)
for segments -> seg, index:
  test.assert(seg == expects[index])

-- trim
--
test.assert("\t foo   \r\n".trim(.ends, " \t\r\n") == 'foo')
test.assert("\t foo   \r\n".trim(.left, " \t\r\n") == "foo   \r\n")
test.assert("\t foo   \r\n".trim(.right, " \t\r\n") == "\t foo")

-- upper/lower
--
test.assert("foo".upper() == "FOO")
test.assert("foo".lower() == "foo")

-- repeat
--
test.assert('*'.repeat(3) == '***')

-- startsWith / endsWith
--
test.assert("foo.cy".startsWith("foo"))
test.assert("foo.cy".endsWith(".cy"))

-- replace
--
test.assert('hello cyber, hello world'.replace("hello", "你好") == "你好 cyber, 你好 world")

-- to bytes
--
var bytes = Array("hello cyber")
test.assert(bytes.len() == 11)
