defmodule CachingService.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      {DynamicSupervisor, strategy: :one_for_one, name: CachingService.MonstersSupervisor},
      CachingService.CharacterRegistry,
      CachingService.MapRegistry
      # CachingService.MonsterRegistry
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: CachingService.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
