defmodule GameService.ConfigFile do
  @moduledoc """
  TODO: Documentation for GameService.ConfigFile

  Note: all functions can raise on invalid files
  """

  @type map_id :: non_neg_integer()

  ## Public API

  @spec map_grid(map_id()) :: {width, height, binary}
        when width: non_neg_integer(), height: non_neg_integer()
  def map_grid(map_id) do
    file = Path.join(priv_dir(), "map_cells/#{map_id}")

    <<width::16-little, height::16-little, map_grid::binary>> = File.read!(file)
    total_size = width * height

    # Just check that the file is valid
    <<_::bytes-size(total_size)>> = map_grid

    {width, height, map_grid}
  end

  @spec map_monsters(map_id()) :: [map()]
  def map_monsters(map_id) do
    file = Path.join(priv_dir(), "map_monster_placement/map_#{map_id}_monsters.yaml")

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

  @spec map_npcs(map_id()) :: [map()]
  def map_npcs(map_id) do
    file = Path.join(priv_dir(), "map_npc_placement/map_#{map_id}_npc.yaml")

    case YamlElixir.read_from_file(file) do
      {:ok, %{"npcs" => npcs}} ->
        npcs
        |> Enum.map(&normalize_npc_entry/1)
        |> Enum.map(&Map.put(&1, :map_id, map_id))

      _ ->
        []
    end
  end

  defp normalize_npc_entry(entry) do
    entry
    |> Enum.flat_map(fn
      {"map_npc_id", v} ->
        [{:id, v}]

      {"vnum", v} ->
        [{:vnum, v}]

      {"pos_x", v} ->
        [{:map_x, v}]

      {"pos_y", v} ->
        [{:map_y, v}]

      {"quest_dialog_id", v} ->
        [{:quest_dialog, v}]

      {"can_move", _} ->
        [{:speed, 0}]

      {"shop_tab_id", v} ->
        [{:shop, v}]

      {"dialog_id", v} ->
        [{:dialog, v}]

      {"direction_facing", v} ->
        [{:direction, v}]

      {"item_shop", v} ->
        [
          {:menu, normalize_npc_menu(v)},
          {:shop, normalize_npc_shop(v)},
          {:inventory, normalize_npc_inventory(v)}
        ]

      {"skill_shop", v} ->
        [
          {:menu, normalize_npc_menu(v)},
          {:shop, %{name: normalize_npc_shop(v)}},
          {:inventory, normalize_npc_inventory(v)}
        ]

      {"effect_vnum", v} ->
        [{:effect, %{vnum: v}}]

      {"effect_delay", v} ->
        [{:effect, %{delay: v}}]
    end)
    |> Map.new()
  end

  defp normalize_npc_menu(shop) do
    %{type: shop["menu_type"]}
  end

  defp normalize_npc_shop(shop) do
    %{
      name: shop["name"],
      shop_type: shop["type"]
    }
  end

  defp normalize_npc_inventory(shop) do
    shop["tabs"]
    |> Enum.map(fn tab ->
      tab
      |> Enum.map(fn
        {"shop_tab_id", v} -> {:id, v}
        {"items", v} -> {:items, normalize_npc_inventory_items(v)}
      end)
      |> Map.new()
    end)
  end

  defp normalize_npc_inventory_items(items) do
    Enum.map(items, fn v ->
      %{
        vnum: Map.get(v, "item_vnum", fn -> Map.fetch!(v, "skill_vnum") end)
      }
    end)
  end

  ## Helpers

  defp priv_dir(), do: :code.priv_dir(:game_service)
end
