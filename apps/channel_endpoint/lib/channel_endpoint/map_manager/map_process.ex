defmodule ChannelEndpoint.MapManager.MapProcess do
  @moduledoc """
  TODO: Documentation
  """

  use GenServer, restart: :temporary

  require Logger

  ## Public API

  @spec start_link(keyword) :: :ignore | {:error, any} | {:ok, pid}
  def start_link(opts) do
    {map_id, gen_options} = Keyword.pop!(opts, :map_id)
    GenServer.start_link(__MODULE__, map_id, gen_options)
  end

  ## GenServer behaviour

  @impl true
  @spec init(pos_integer) :: {:ok, nil, {:continue, :init_maps}}
  def init(map_id) do
    Logger.info("MapProcess##{map_id} started")
    {:ok, map_id, :hibernate}
  end
end
