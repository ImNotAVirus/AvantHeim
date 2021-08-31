defmodule CachingService.MonsterRegistry.MonsterProcess do
  @moduledoc """
  TODO: Documentation
  """

  use GenServer

  require Logger

  ## Public API

  @spec start_link(pos_integer) :: :ignore | {:error, any} | {:ok, pid}
  def start_link(monster_id) do
    GenServer.start_link(__MODULE__, monster_id, name: :"#{__MODULE__}##{monster_id}")
  end

  ## GenServer behaviour

  @impl true
  def init(monster_id) do
    Logger.debug("MonsterProcess ##{monster_id} started")
    {:ok, monster_id, :hibernate}
  end
end
