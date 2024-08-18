defmodule GameService.GameConfig do
  @moduledoc """
  TODO: Documentation for GameService.GameConfig

  Note: functions can raise on invalid files

  map_cells table tuple :
  - {id, width, height, map_grid}

  map_info table tuple :
  - {id, vnum, name_id, music_id, base_map}

  """

  require Logger

  alias GameService.Structures.PortalStructure

  @init_timeout :timer.seconds(30)
  @base_dir Application.compile_env(:game_service, :config_dir, :code.priv_dir(:game_service))

  ## Public API

  def init() do
    _ = create_tables()

    Task.await_many(
      [
        Task.async(fn -> {map_info_table(), load_map_info()} end),
        Task.async(fn -> {map_cells_table(), load_map_cells()} end),
        Task.async(fn -> {map_portals_table(), load_map_portals()} end)
      ],
      @init_timeout
    )
  end

  def clean() do
    :ets.delete(map_info_table())
    :ets.delete(map_cells_table())
    :ets.delete(map_portals_table())
  end

  def maps_info() do
    :ets.tab2list(map_info_table())
  end

  def static_map_info_ids() do
    map_info_table()
    # {id, vnum, name_id, music_id, "IS_BASE_MAP" in flags}
    |> :ets.match({:"$1", :_, :_, :_, true})
    |> List.flatten()
  end

  def map_monsters(map_id) do
    file = map_monsters_file(map_id)

    case YamlElixir.read_from_file(file) do
      {:ok, %{"monsters" => monsters}} ->
        monsters
        |> Enum.map(&Map.take(&1, ~w(id vnum map_x map_y)))
        |> Enum.map(&Map.new(&1, fn {k, v} -> {String.to_atom(k), v} end))
        |> Enum.map(&Map.put(&1, :map_id, map_id))

      _ ->
        []
    end
  end

  def map_portals(map_id) do
    map_portals_table()
    |> :ets.lookup(map_id)
    |> Enum.map(&elem(&1, 1))
  end

  ## Helpers

  defp map_info_table(), do: :map_info
  defp map_cells_table(), do: :map_cells
  defp map_portals_table(), do: :map_portals

  defp map_info_files(), do: Path.join(@base_dir, "maps/*.yaml")
  defp map_cells_files(), do: Path.join(@base_dir, "map_cells/*")
  defp map_portals_files(), do: Path.join(@base_dir, "map_portals/portals_*.yaml")

  defp map_monsters_file(map_id) do
    Path.join(@base_dir, "map_monster_placement/map_#{map_id}_monsters.yaml")
  end

  defp create_tables() do
    _ =
      :ets.new(map_info_table(), [
        :set,
        :public,
        :named_table,
        read_concurrency: true,
        write_concurrency: :auto
      ])

    _ =
      :ets.new(map_cells_table(), [
        :set,
        :public,
        :named_table,
        read_concurrency: true,
        write_concurrency: :auto
      ])

    _ =
      :ets.new(map_portals_table(), [
        :bag,
        :public,
        :named_table,
        read_concurrency: true,
        write_concurrency: :auto
      ])
  end

  defp load_map_info() do
    map_info_files()
    |> Path.wildcard()
    |> Stream.flat_map(&parse_map_info_file!/1)
    |> Stream.map(fn tuple ->
      case :ets.insert_new(map_info_table(), tuple) do
        true -> :ok
        false -> raise "duplicate map info with id #{elem(tuple, 0)}"
      end
    end)
    |> Enum.count()
  end

  defp load_map_cells() do
    map_cells_files()
    |> Path.wildcard()
    |> Enum.map(&parse_map_cell_file!/1)
    |> Enum.map(fn tuple ->
      case :ets.insert_new(map_cells_table(), tuple) do
        true -> :ok
        false -> raise "duplicate map cells with id #{elem(tuple, 0)}"
      end
    end)
    |> Enum.count()
  end

  defp load_map_portals() do
    map_portals_files()
    |> Path.wildcard()
    |> Stream.flat_map(&parse_map_portal_file!/1)
    |> Stream.map(&:ets.insert(map_portals_table(), &1))
    |> Enum.count()
  end

  defp parse_map_info_file!(file) do
    # {id, vnum, name_id, music_id, base_map, flags}
    file
    |> YamlElixir.read_from_file!()
    |> List.wrap()
    |> Enum.map(&Map.take(&1, ~w(map_id map_vnum map_name_id map_music_id flags)))
    |> Enum.map(fn info ->
      %{
        "map_id" => id,
        "map_vnum" => vnum,
        "map_name_id" => name_id,
        "map_music_id" => music_id
      } = info

      # Flags can be nil or non existant
      flags = info["flags"] || []

      {id, vnum, name_id, music_id, "IS_BASE_MAP" in flags}
    end)
  end

  defp parse_map_cell_file!(file) do
    id = file |> Path.basename(Path.extname(file)) |> String.to_integer()

    <<width::16-little, height::16-little, map_grid::binary>> = File.read!(file)
    total_size = width * height

    # Just check that the file is valid
    <<_::bytes-size(total_size)>> = map_grid

    {id, width, height, map_grid}
  end

  defp parse_map_portal_file!(file) do
    keys = ~w(
      source_map_id source_map_x source_map_y
      destination_map_id destination_map_x destination_map_y
      type
    )

    map_id =
      file
      |> Path.basename(".yaml")
      |> then(fn "portals_" <> map_vnum -> map_vnum end)
      |> String.to_integer(10)

    file
    |> YamlElixir.read_from_file!()
    |> Map.fetch!("portals")
    |> Enum.map(&Map.take(&1, keys))
    |> Enum.map(fn info ->
      %{
        "source_map_id" => ^map_id,
        "source_map_x" => source_map_x,
        "source_map_y" => source_map_y,
        "destination_map_id" => destination_map_id,
        "destination_map_x" => destination_map_x,
        "destination_map_y" => destination_map_y,
        "type" => type
      } = info

      {
        map_id,
        %PortalStructure{
          source_map_id: map_id,
          source_map_x: source_map_x,
          source_map_y: source_map_y,
          destination_map_id: destination_map_id,
          destination_map_x: destination_map_x,
          destination_map_y: destination_map_y,
          type: type
        }
      }
    end)
  catch
    kind, payload ->
      Logger.error("""
      Unable to parse #{file}:
        #{Exception.format(kind, payload, __STACKTRACE__)}
      """)

      []
  end
end
