# Used by "mix format"

file_ignored = [
  "lib/channel_endpoint/endpoint/packets/clist_packet.ex"
]

inputs =
  ["{mix,.formatter}.exs", "#{__DIR__}/{config,lib,test}/**/*.{ex,exs}"]
  |> Enum.flat_map(&Path.wildcard(&1, match_dot: true))
  |> Enum.map(&String.replace_leading(&1, "#{__DIR__}/", ""))
  |> Kernel.--(file_ignored)

[
  # inputs: ["{mix,.formatter}.exs", "{config,lib,test}/**/*.{ex,exs}"],
  inputs: inputs,
  import_deps: [:core]
]
