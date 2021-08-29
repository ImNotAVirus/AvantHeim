defmodule ChannelEndpoint.Endpoint.UIActions do
  @moduledoc """
  TODO: Documentation
  """

  alias Core.Socket
  alias CachingService.Player.Character
  alias ChannelEndpoint.Endpoint.EntityInteractions
  alias ChannelEndpoint.Endpoint.UIViews

  import DatabaseService.EntityEnums, only: [entity_type: 2]

  ## Packet handlers

  @emoji_offset 4099
  @rainbow_vomit_vnum 5116

  @spec show_emoji(String.t(), map, Socket.t()) :: {:cont, Socket.t()}
  def show_emoji("guri", params, %Socket{} = socket) do
    %{guri_data: guri_data, entity_id: entity_id, entity_type: entity_type} = params
    entity = CachingService.get_entity_by_id(entity_type, entity_id)

    case entity do
      {:ok, character} ->
        case guri_data do
          x when x >= 973 and x <= 999 ->
            EntityInteractions.show_effect(character, guri_data + @emoji_offset)

          1000 ->
            EntityInteractions.show_effect(character, @rainbow_vomit_vnum)

          x ->
            Socket.send(socket, UIViews.render(:info, %{message: "UNAUTHORIZED_EMOTICON"}))
        end
    end

    {:cont, socket}
  end
end
