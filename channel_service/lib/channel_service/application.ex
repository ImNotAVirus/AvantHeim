defmodule ChannelService.Application do
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      {ChannelService.Endpoint, name: ChannelService.Endpoint}
    ]

    opts = [strategy: :one_for_one, name: ChannelService.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
