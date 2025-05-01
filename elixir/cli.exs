#!/usr/bin/env elixir

help_info =
  ~S"""

  USAGE: app <OPTIONS>

  OPTIONS:
      --help,-h                Print help info
      --name      <string>     Name to greet
      --times,-n  <integer=1>  Times to greet

  EXAMPLES:
      > app --name elixir
      > app --name elixir --times 3
  """

spec = [
  aliases: [n: :times, h: :help],
  strict: [help: :boolean, name: :string, times: :integer]
]

argv = System.argv()
{parsed, _} = OptionParser.parse!(argv, spec)

if parsed == [] || parsed[:help] do
  IO.puts(help_info)
  exit(:normal)
end

name = parsed[:name] || raise("--name : Missing")
times = parsed[:times] || 1

for _ <- 1..times do
  IO.puts("Hi #{name}.")
end
