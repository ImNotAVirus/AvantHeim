# Used by "mix format"

# Ignore `lib/uuid.ex` file
inputs =
  ["{mix,.formatter}.exs", "#{__DIR__}/{config,lib,test}/**/*.{ex,exs}"]
  |> Enum.flat_map(&Path.wildcard(&1, match_dot: true))
  |> Enum.map(&String.replace_leading(&1, "#{__DIR__}/", ""))
  |> Kernel.--(["lib/uuid.ex"])

[
  # inputs: ["{mix,.formatter}.exs", "{config,lib,test}/**/*.{ex,exs}"]
  inputs: inputs,
  export: [
    locals_without_parens: [
      ignore_packet: 1,
      packet: 2,
      field: 2,
      field: 3,
      resolve: 2
    ]
  ]
]
