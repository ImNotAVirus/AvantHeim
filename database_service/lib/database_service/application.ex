defmodule DatabaseService.Application do
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [DatabaseService.Repo]
    opts = [strategy: :one_for_one, name: DatabaseService.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
