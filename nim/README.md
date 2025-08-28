# Nim Lang Learning

## Resources

- https://nim-lang.org/docs/manual.html
- https://nim-lang.github.io/nimony-website/index.html
- https://nimprogramming.com/tutorials/all
- https://internet-of-tomohiro.netlify.app/nim/faq.en
- https://nim-lang.org/araq/
- https://forum.nim-lang.org/t/10080#66502 (streaming IO still an issue)

## Nim Script


| function       | nims                                   | nim                               |
|----------------|----------------------------------------|-----------------------------------|
| env            | system/nimscript.getEnv()              | std/envvar.getEnv()               |
| cmdline param  | system/nimscript.paramCount/paramStr() | std/cmdline.paramCount/paramStr() |
| fitler/toSeq   | std/sugar.collect()                    | std/sequtils                      |
| get cmd result | system.gorge()/gorgeEx()               | std/osproc                        |
| time           | get from shell                         | std/time                          |


> NOTE:
> It is **not recommended** to use Nim script for heavy task, because the memory overhead of the VM is large, at least on Windows.

__Consider using Lua/Fennel/Janet/Babashka ;)__
