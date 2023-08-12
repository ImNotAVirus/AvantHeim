defmodule ChannelService.Endpoint.ChatActions do
  @moduledoc """
  TODO: Documentation
  """

  alias ElvenGard.Network.Socket
  alias ChannelService.Endpoint.EntityInteractions
  alias ElvenCaching.CharacterRegistry

  ## Packet handlers

  @spec player_general_chat(String.t(), map, Socket.t()) :: {:cont, Socket.t()}
  def player_general_chat("say", %{message: message}, %Socket{} = socket) do
    if String.starts_with?(message, "!") do
      raise "TODO: unimplemented timespace chat"
    end

    %{character_id: character_id} = socket.assigns
    {:ok, character} = CharacterRegistry.get(character_id)

    EntityInteractions.say_to_map(character, message)

    {:cont, socket}
  end
end
