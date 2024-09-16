#!/usr/bin/env cyber

use os
use test

var cpu = os.cpu
var endian = os.endian
var sys = os.system
var cwd = os.cwd()
var exe_path = os.exePath()
var monotime = os.now()
var unixtime = os.milliTime()

print "CPU      : $(cpu)"
print "ENDIAN   : $(endian)"
print "SYSTEM   : $(sys)"
print "CWD      : $(cwd)"
print "EXEPATH  : $(exe_path)"
print "MONOTIME : $(monotime)"
print "UNIXTIME : $(unixtime)"

-- get all env vars map
-- for os.getEnvAll() -> [key, value]:
--   print "env   : $(key) => $(value)"

if os.getEnv("SHELL") -> sh:
  print "SHELL    : $(sh)"
else:
  print "SHELL NOT FOUND"

-- args
--
for os.args() -> arg:
  print "ARG      : $(arg)"

-- real path (resolve link)
--
print os.realPath('/lib')


-- write file
--
os.writeFile('temp.txt', 'hello cyber')

-- read file
--
test.assert(os.readFile('temp.txt') == 'hello cyber')

-- remove file
--
os.removeFile('temp.txt')

-- execCmd
--
var cmd = List.fill('', 2)
cmd[0] = 'cat'
cmd[1] = '/etc/hostname'
var res = os.execCmd(cmd)
test.assert( res['exited'] == 0 )
var hostname = res['out']
test.assert(res['err'] == '')

print "HOSTNAME : $(hostname)"
