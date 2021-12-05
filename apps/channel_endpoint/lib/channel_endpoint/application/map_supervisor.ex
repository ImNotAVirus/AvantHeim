defmodule ChannelEndpoint.Application.MapSupervisor do
  @moduledoc """
  TODO: Documentation
  """

  use Supervisor

  ## Public API

  def start_link(opts) do
    Supervisor.start_link(__MODULE__, nil, opts)
  end

  ## Supervisor behaviour

  @impl true
  def init(_init_arg) do
    children = [
      {ChannelEndpoint.MapManager, []},
      {DynamicSupervisor, strategy: :one_for_one, name: ChannelEndpoint.MapProcesses}
    ]

    Supervisor.init(children, strategy: :one_for_all)
  end
end
