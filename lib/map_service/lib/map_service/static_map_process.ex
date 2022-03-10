defmodule MapService.StaticMapProcess do
  @moduledoc false

  use GenServer

  require Logger

  alias MapService.ConfigFile.MapConfig

  # @typep state :: %{config: MapConfig.t(), character_ids: MapSet.t(pos_integer)}

  ## Public API

  @spec start_link(MapConfig.t(), GenServer.options()) :: GenServer.on_start()
  def start_link(config, process_opts) do
    GenServer.start_link(__MODULE__, config, process_opts)
  end

  ## GenServer behaviour

  @impl true
  def init(config) do
    Logger.debug("StaticMapProcess##{config.id} started")
    {:ok, %{config: config, character_ids: MapSet.new()}, :hibernate}
  end
end
