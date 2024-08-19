defmodule GameService.InitMapSystemTest do
  # FIXME: Rewrite this module
  # Currently systems and game service are automatically spawned
  # Need to rewrite using `test --no-start`
  use GameService.SystemCase, async: true

  # alias GameService.InitMapSystem

  ## Tests

  describe "run/1" do
    # @tag capture_log: true
    # test "persist the map grid", %{map_id: map_id} do
    #   map_grid = {2, 3, <<1, 2, 3, 4, 5, 6>>}

    #   # Patch ConfigFile
    #   patch(GameService.GameConfig, :map_grid, fn ^map_id -> map_grid end)

    #   # Call our system
    #   _ = InitMapSystem.run(%{partition: map_id})

    #   # Grid should be persisted
    #   assert ^map_grid = :persistent_term.get({:map_grid, map_id})
    # end

    test "load monsters" do
      map_id = 1
      monsters = GameService.GameConfig.map_monsters(map_id)

      # Call our system
      # _ = InitMapSystem.run(%{partition: map_id})

      # Entities should exists in the system
      Enum.each(monsters, fn %{id: monster_id} ->
        assert {:ok, _} = Query.fetch_entity(GameService.real_entity_id(:monster, monster_id))
      end)
    end
  end
end
