defmodule ChannelEndpoint.Endpoint.ChatActions do
  @moduledoc """
  TODO: Documentation
  """

  alias Core.Socket
  alias ChannelEndpoint.Endpoint.EntityInteractions

  ## Packet handlers

  @spec player_general_chat(String.t(), map, Socket.t()) :: {:cont, Socket.t()}
  def player_general_chat("say", %{message: message}, %Socket{} = socket) do
    if String.starts_with?(message, "!") do
      raise "unimplemented timespace chat"
    end

    %{character_id: character_id} = socket.assigns
    {:ok, character} = CachingService.get_character_by_id(character_id)

    EntityInteractions.player_say_ui(character, message)

    {:cont, socket}
  end
end
