defmodule SessionService.Application do
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [SessionService.Worker]
    opts = [strategy: :one_for_one, name: SessionService.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
