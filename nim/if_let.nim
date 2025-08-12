let name = "foo"
if (let len = name.len; len < 5):
  echo name.repr, " len < 5", " len=", len
else:
  echo name.repr, " len >= 5"
