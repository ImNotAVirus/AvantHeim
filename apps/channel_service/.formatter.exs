# Used by "mix format"
[
  inputs: ["{mix,.formatter}.exs", "{config,lib,test}/**/*.{ex,exs}"],
  import_deps: [:elvengard_network],
  locals_without_parens: [
    defcommand: 2
  ]
]
