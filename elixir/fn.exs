#!/usr/bin/env elixir

# => &System.get_env/1
f = Function.capture(System, :get_env, 1)

# info
Function.info(f)
Function.info(f, :type)

# apply
apply(f, ["TMPDIR"])
apply(&System.get_env/1, ["TMPDIR"])
apply(System, :get_env, ["TMPDIR"])
