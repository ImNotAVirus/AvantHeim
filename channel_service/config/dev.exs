import Config

## Clustering strategy

config :libcluster,
  topologies: [
    local_epmd: [
      strategy: Elixir.Cluster.Strategy.LocalEpmd
    ]
  ]
