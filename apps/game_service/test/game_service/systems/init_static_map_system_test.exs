defmodule GameService.InitStaticMapSystemTest do
  use GameService.SystemCase, async: true
  use Patch

  alias GameService.InitStaticMapSystem

  ## Setup

  setup do
    map_id = Enum.random(1..999_999)

    # Defaults patchs
    patch(GameService.ConfigFile, :map_grid, fn ^map_id -> {0, 0, <<>>} end)
    patch(GameService.ConfigFile, :map_monsters, fn ^map_id -> [] end)

    %{map_id: map_id}
  end

  ## Tests

  describe "run/1" do
    @tag capture_log: true
    test "persist the map grid", %{map_id: map_id} do
      map_grid = {2, 3, <<1, 2, 3, 4, 5, 6>>}

      # Patch ConfigFile
      patch(GameService.ConfigFile, :map_grid, fn ^map_id -> map_grid end)

      # Call our system
      _ = InitStaticMapSystem.run(%{partition: map_id})

      # Grid should be persisted
      assert ^map_grid = :persistent_term.get({:map_grid, map_id})
    end

    test "load monsters", %{map_id: map_id} do
      ref1 = make_ref()
      ref2 = make_ref()

      monsters = [
        %{id: ref1, vnum: 1, map_id: 2, map_x: 3, map_y: 4},
        %{id: ref2, vnum: 5, map_id: 6, map_x: 7, map_y: 8}
      ]

      # Patch ConfigFile
      patch(GameService.ConfigFile, :map_monsters, fn ^map_id -> monsters end)

      # Call our system
      _ = InitStaticMapSystem.run(%{partition: map_id})

      # Entities should exists in the system
      assert {:ok, _} = Query.fetch_entity(GameService.real_entity_id(:monster, ref1))
      assert {:ok, _} = Query.fetch_entity(GameService.real_entity_id(:monster, ref2))
    end
  end
end
