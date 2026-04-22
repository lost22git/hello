#!/usr/bin/env janet

(comment
  # === PEG ===
  # https://janet-lang.org/docs/peg.html
  # - primitive patterns
  # - combining patterns
  # - captures
  # - recursion

  (def hexcolor
    ~{:hex (range "09" "af" "AF") # :h
      :hexhex (2 :hex)
      :capture-hexhex (<- :hexhex)
      :capture-hexhex-with-pos (*
                                 ($)
                                 :capture-hexhex
                                 ($))
      :main (*
              "#"
              (3 :capture-hexhex-with-pos))})

  (peg/match hexcolor "#FF800F")

  # end comment
  )
