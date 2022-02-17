defmodule ChannelService.Endpoint.UIActions do
  @moduledoc """
  TODO: Documentation
  """

  alias ElvenCore.Socket
  alias ChannelService.Endpoint.EntityInteractions
  alias ChannelService.Endpoint.UIViews

  ## Packet handlers

  @emoji_offset 4099
  @rainbow_vomit_vnum 5116

  @spec show_emoji(String.t(), map, Socket.t()) :: {:cont, Socket.t()}
  def show_emoji("guri", params, %Socket{} = socket) do
    %{guri_data: guri_data, entity_id: entity_id, entity_type: entity_type} = params
    entity = CachingService.get_entity_by_id(entity_type, entity_id)

    case {entity, guri_data} do
      {{:ok, e}, g} when g >= 973 and g <= 999 ->
        EntityInteractions.show_effect(e, guri_data + @emoji_offset)

      {{:ok, e}, 1000} ->
        EntityInteractions.show_effect(e, @rainbow_vomit_vnum)

      {{:ok, _e}, _} ->
        Socket.send(socket, UIViews.render(:info, %{message: "UNAUTHORIZED_EMOTICON"}))

      _ ->
        :ok
    end

    {:cont, socket}
  end
end
