#!/usr/bin/env raku
my %*SUB-MAIN-OPTS =
:named-anywhere,             # allow named variables at any location
:bundling,                   # allow bundling of named arguments
:coerce-allomorphs-to(Int),  # coerce allomorphic arguments to given type
:allow-no,                   # allow --no-foo as alternative to --/foo
:numeric-suffix-as-value,    # allow -j2 as alternative to --j=2
;

unit sub MAIN(
  Str $name where *.chars > 3,    #= name to greet
  UInt :n(:$times) = 1,           #= times of greeting
  Bool :v(:$verbose),             #= verbose
);

say "> name: $name, times: $times" if ?$verbose;
say "Hello, $name" for (1..$times);
  
