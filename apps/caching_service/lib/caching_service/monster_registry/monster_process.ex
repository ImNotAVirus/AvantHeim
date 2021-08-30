defmodule CachingService.MonsterRegistry.MonsterProcess do
  @moduledoc """
  TODO: Documentation
  """

  use GenServer

  require Logger

  alias CachingService.MonsterRegistry.MapMonster

  ## Public API

  @spec start_link(MapMonster.t()) :: :ignore | {:error, any} | {:ok, pid}
  def start_link(%MapMonster{} = monster_specs) do
    GenServer.start_link(__MODULE__, monster_specs, name: :"#{__MODULE__}##{monster_specs.id}")
  end

  ## GenServer behaviour

  @impl true
  def init(%MapMonster{} = monster_specs) do
    Logger.debug("MonsterProcess ##{monster_specs.id} started")
    {:ok, nil, :hibernate}
  end
end
