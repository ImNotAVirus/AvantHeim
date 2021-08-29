defmodule ChannelEndpoint.Endpoint.UIActions do
  @moduledoc """
  TODO: Documentation
  """

  alias Core.Socket
  alias CachingService.Player.Character
  alias ChannelEndpoint.Endpoint.EntityInteractions

  import DatabaseService.EntityEnums, only: [entity_type: 2]

  ## Packet handlers

  @spec show_emoji(String.t(), map, Socket.t()) :: {:cont, Socket.t()}
  def show_emoji("guri", params, %Socket{} = socket) do
    %{guri_data: guri_data, entity_id: entity_id} = params

    {:ok, character} = CachingService.get_character_by_id(entity_id)

    emoji_offset =
      case guri_data do
        x when x >= 973 and x <= 999 -> 4099
        x when x == 1000 -> 4116
        x -> x
      end

    value = guri_data + emoji_offset

    if guri_data >= 973 and guri_data <= 1000 do
      EntityInteractions.show_effect(character, value)
    end

    {:cont, socket}
  end
end
