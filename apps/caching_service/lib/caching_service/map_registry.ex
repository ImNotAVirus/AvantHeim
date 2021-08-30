defmodule CachingService.MapRegistry do
  @moduledoc """
  TODO: Documentation
  """

  use GenServer

  require Logger

  @maps_path "#{:code.priv_dir(:caching_service)}/client_files/maps"

  ## Public API

  @spec start_link(any) :: :ignore | {:error, any} | {:ok, pid}
  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  ## GenServer behaviour

  @impl true
  def init(opts) do
    map_id = Keyword.fetch!(opts, :map_id)
    Logger.info("CharacterMap (id=#{map_id}) starting...")
    {:ok, %{map_id: map_id}, {:continue, :init_map}}
  end

  @impl true
  def handle_continue(:init_map, %{map_id: map_id}) do
    filename = "#{@maps_path}/#{map_id}"
    <<width::16-little, height::16-little, map_bin::binary>> = File.read!(filename)

    tensor =
      map_bin
      |> Nx.from_binary({:s, 8})
      |> Nx.reshape({height, width}, names: [:height, :width])

    Logger.info("CharacterMap (id=#{map_id} size=#{width}x#{height}) started")

    new_state = %{map_id: map_id, width: width, height: height, tensor: tensor}
    {:noreply, new_state}
  end
end
