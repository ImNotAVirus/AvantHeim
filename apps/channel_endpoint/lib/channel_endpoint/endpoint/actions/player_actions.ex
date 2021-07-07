defmodule ChannelEndpoint.Endpoint.PlayerActions do
  @moduledoc """
  TODO: Documentation
  """
  
  alias ChannelEndpoint.Endpoint.PlayerViews
  alias Core.Socket
  
  @spec game_start(String.t(), map, Socket.t()) :: {:cont, Socket.t()}
  def game_start("game_start", _, %Socket{} = socket) do
    IO.inspect(socket.assigns)
    
    %{character_id: character_id} = socket.assigns
    character = FakeData.character(id: character_id)

    Socket.send(socket, PlayerViews.render(:tit, character))
    
    {:cont, socket}
  end
end
