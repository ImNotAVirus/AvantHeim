defmodule ChannelService.GameActions do
  @moduledoc """
  TODO: Documentation
  """

  alias ElvenGard.Network.Socket
  alias ElvenGard.ECS.Command
  alias GameService.PlayerEntity

  ## Packet handlers

  @spec game_start(String.t(), map, Socket.t()) :: {:cont, Socket.t()}
  def game_start("game_start", _, %Socket{} = socket) do
    %{character: character, account: account} = socket.assigns

    {:ok, _entity} = Command.spawn_entity(PlayerEntity.new(character, account, self()))
    {:cont, Map.update!(socket, :assigns, &Map.delete(&1, :character))}
  end
end
