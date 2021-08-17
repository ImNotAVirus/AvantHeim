defmodule ChannelEndpoint.Endpoint.PacketHelpers do
  @moduledoc """
  TODO: Break theses functions into another modules
  """

  alias Core.Socket
  alias CachingService.Player.Character

  alias ChannelEndpoint.Endpoint.{
    EntityViews,
    MapViews,
    PlayerViews
  }

  @spec player_map_enter(Character.t(), Socket.t()) :: :ok | {:error, atom}
  def player_map_enter(%Character{} = character, %Socket{} = socket) do
    Socket.send(socket, PlayerViews.render(:c_info, character))
    Socket.send(socket, EntityViews.render(:c_mode, character))
    Socket.send(socket, PlayerViews.render(:lev, character))
    Socket.send(socket, PlayerViews.render(:stat, character))
    Socket.send(socket, MapViews.render(:at, character))
    Socket.send(socket, MapViews.render(:c_map, character))
    # TODO: Socket.send(socket, PlayerViews.render(:sc, character))
    Socket.send(socket, EntityViews.render(:char_sc, character))
    Socket.send(socket, EntityViews.render(:cond, character))
  end
end
