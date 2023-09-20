defmodule ChannelService.GameActions do
  @moduledoc """
  TODO: Documentation
  """

  alias ElvenGard.Network.Socket
  alias ElvenGard.ECS.Command
  alias GameService.PlayerBundle
  alias GameService.Events.EntitySpawned

  ## Packet handlers

  @spec game_start(String.t(), map, Socket.t()) :: {:cont, Socket.t()}
  def game_start("game_start", _, %Socket{} = socket) do
    %{character: character, account: account} = socket.assigns

    # Spawn Player entity into system
    specs = PlayerBundle.new(character, account, self())
    {:ok, {entity, components}} = Command.spawn_entity(specs)

    # Send a spawn event to the map partition
    {:ok, _events} =
      ElvenGard.ECS.push(
        %EntitySpawned{entity: entity, components: components},
        # You have to use map_ref instead of map_id but here the player just logged
        # so map_ref = map_id 
        partition: character.map_id
      )

    {:cont, Map.update!(socket, :assigns, &Map.delete(&1, :character))}
  end
end
