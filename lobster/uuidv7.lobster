#!/usr/bin/env lobster

class Uuid:
  inner: [int]

// new Uuid (verison 7) 
def uuidv7():
  let unix_ms = date_time(true)[0] * 1000
  return uuidv7(unix_ms)

// new Uuid (verison 7) with given unix_ms (unix timestamp milliseconds)
def uuidv7(unix_ms:int):
  let bs = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
  for(6) i:
    bs[i] = unix_ms >> (8*(5-i)) & 0xff 
  for(10) i:
    bs[6+i] = rnd(256) 
  bs[6] = bs[6] & 0x0F | 0x70
  bs[8] = bs[8] & 0x3F | 0x80
  return Uuid{inner:bs}

// hex stringify Uuid
def to_string(_ :: Uuid, sep:string=""):
  var s = ""
  for(inner) b,i:
    if i == 4 or i == 6 or i == 8 or i == 10:
      s += sep
    s += number_to_string(b,16,2)
  return s

// read version from Uuid (verison 7)
def version(_ :: Uuid):
  return 7

// read variant from Uuid (verison 7)
def variant(_ :: Uuid):
  return 2

// read unix_ms from Uuid (verison 7)
def unix_ms(_ :: Uuid):
  var r = 0
  for(6) i:
    r = (r << 8) + inner[i]
  return r


/////////////
// Testing //
/////////////

import testing

run_test("new uuidv7"):
    assert uuidv7() != nil

run_test("new uuidv7 with given unix_ms"):
    assert uuidv7(date_time(true)[0] * 1000) != nil

run_test("uuid to string"):
    assert uuidv7().to_string() != nil
    assert length(uuidv7().to_string()) == 32
    assert uuidv7().to_string("-") != nil
    assert length(uuidv7().to_string("-")) == 36
    assert find_string(uuidv7().to_string("-"), "-") > 0

run_test("uuid read unix_ms"):
    let unix_ms = date_time(true)[0] * 1000
    assert uuidv7(unix_ms).unix_ms() == unix_ms

print(uuidv7().to_string())
print(uuidv7().to_string("-"))
