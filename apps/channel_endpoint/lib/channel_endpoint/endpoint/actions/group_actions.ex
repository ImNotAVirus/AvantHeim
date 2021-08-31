defmodule ChannelEndpoint.Endpoint.GroupActions do
  @moduledoc """
  TODO: Documentation
  """

  alias Core.Socket
  alias ChannelEndpoint.Endpoint.{UIViews, PlayerViews}

  import ChannelEndpoint.GroupRequestEnums, only: [group_request_type: 2]

  ## Packet handlers

  @spec create_group(String.t(), map, Socket.t()) :: {:cont, Socket.t()}
  def create_group(
        "pjoin",
        %{request_type: request_type, entity_id: entity_id},
        %Socket{} = socket
      ) do
    %{character_id: character_id} = socket.assigns
    {:ok, character} = CachingService.get_character_by_id(character_id)

    maybe_character = CachingService.get_character_by_id(entity_id)

    case {maybe_character, request_type} do
      {{:ok, c}, r} ->
        case r do
          group_request_type(:requested, :value) ->
            # i18n string 234 = {PlayerName} has been requested to join
            Socket.send(
              character.socket,
              UIViews.render(:infoi2, %{i18n_vnum: 234, params_count: 1, entity: c})
            )

            # i18n string 233 = {PlayerName} has invited you to join their party
            Socket.send(
              c.socket,
              UIViews.render(:dlgi2, %{
                packet_yes: PlayerViews.render(:pjoin, %{entity: c, request_type: :accepted}),
                packet_no: PlayerViews.render(:pjoin, %{entity: c, request_type: :declined}),
                i18n_vnum: 233,
                params_count: 1,
                name: character.name
              })
            )

          group_request_type(:accepted, :value) ->
            :ok

          group_request_type(:declined, :value) ->
            :ok

          _ ->
            :ok
        end

      {_, _} ->
        :ok
    end

    {:cont, socket}
  end
end
