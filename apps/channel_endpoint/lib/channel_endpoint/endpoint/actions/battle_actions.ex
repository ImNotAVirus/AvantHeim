defmodule ChannelEndpoint.Endpoint.BattleActions do
  @moduledoc """
  TODO: Documentation
  """

  alias Core.Socket
  alias ChannelEndpoint.Endpoint.UIViews

  ## Packet handlers

  @spec use_skill(String.t(), map, Socket.t()) :: {:cont, Socket.t()}
  def use_skill("u_s", _args, %Socket{} = socket) do
    %{character_id: character_id} = socket.assigns
    {:ok, character} = CachingService.get_character_by_id(character_id)
    Socket.send(socket, UIViews.render(:cancel, %{type: 2, entity: character}))
    {:cont, socket}
  end
end
