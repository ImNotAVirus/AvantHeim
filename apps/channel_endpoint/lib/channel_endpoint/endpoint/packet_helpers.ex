defmodule ChannelEndpoint.Endpoint.PacketHelpers do
  @moduledoc """
  TODO: Break theses functions into another modules
  """

  alias Core.Socket
  alias CachingService.Position
  alias CachingService.Player.Character

  alias ChannelEndpoint.Endpoint.{
    EntityViews,
    MapViews,
    PlayerViews,
    VisibilityViews
  }

  @spec map_enter(Character.t()) :: :ok | {:error, atom}
  def map_enter(%Character{} = character) do
    ## Self packets
    Socket.send(character.socket, PlayerViews.render(:c_info, character))
    Socket.send(character.socket, EntityViews.render(:c_mode, character))
    Socket.send(character.socket, PlayerViews.render(:lev, character))
    Socket.send(character.socket, PlayerViews.render(:stat, character))
    Socket.send(character.socket, MapViews.render(:at, character))
    Socket.send(character.socket, MapViews.render(:c_map, character))
    # TODO: Socket.send(character.socket, PlayerViews.render(:sc, character))
    Socket.send(character.socket, EntityViews.render(:char_sc, character))
    Socket.send(character.socket, EntityViews.render(:cond, character))

    ## Other players packets
    %Position{map_id: map_id} = Character.get_position(character)

    {:ok, players} = CachingService.get_characters_by_map_id(map_id, [{:!==, :id, character.id}])
    Enum.each(players, &send_visibility_packets(character, &1))
  end

  @spec set_speed(Character.t()) :: :ok | {:error, atom}
  def set_speed(%Character{} = character) do
    Socket.send(character.socket, EntityViews.render(:cond, character))
  end

  ## Private functions

  @spec send_visibility_packets(Character.t(), Character.t()) :: :ok | {:error, atom}
  def send_visibility_packets(self, character) do
    Socket.send(self.socket, VisibilityViews.render(:in, character))
    Socket.send(character.socket, VisibilityViews.render(:in, self))
  end
end
