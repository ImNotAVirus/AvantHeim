defmodule ChannelEndpoint.Endpoint.GroupActions do
  @moduledoc """
  TODO: Documentation
  """

  alias Core.Socket
  alias CachingService.Player.Character
  alias ChannelEndpoint.Endpoint.{UIViews, PlayerViews, ChatViews}

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

    maybe_target = CachingService.get_character_by_id(entity_id)

    case {maybe_target, request_type} do
      {{:ok, target}, r} ->
        case r do
          x
          when x == group_request_type(:requested, :value) or
                 x == group_request_type(:invited, :value) ->
            send_group_invitation(&send_ui_invitation/2, character, target)

          group_request_type(:accepted, :value) ->
            :ok

          group_request_type(:declined, :value) ->
            # {PlayerName} rejected your invitation
            Socket.send(
              target.socket,
              ChatViews.render(:sayi2, %{
                entity: character,
                color: :special_gold,
                i18n_vnum: 237,
                params_count: 1,
                name: character.name
              })
            )

          _ ->
            :ok
        end

      {_, _} ->
        :ok
    end

    {:cont, socket}
  end

  # Private function

  defp send_ui_invitation(%Character{} = character, %Character{} = target) do
    # i18n string 234 = {PlayerName} has been requested to join
    Socket.send(
      character.socket,
      UIViews.render(:infoi2, %{i18n_vnum: 234, params_count: 1, entity: target})
    )

    # i18n string 233 = {PlayerName} has invited you to join their party
    Socket.send(
      target.socket,
      UIViews.render(:dlgi2, %{
        packet_yes: PlayerViews.render(:pjoin, %{entity: character, request_type: :accepted}),
        packet_no: PlayerViews.render(:pjoin, %{entity: character, request_type: :declined}),
        i18n_vnum: 233,
        params_count: 1,
        name: character.name
      })
    )
  end

  defp send_group_invitation(callback, %Character{} = character, %Character{} = target) do
    case {character, target} do
      {x, y} when x.id == y.id ->
        Socket.send(
          character.socket,
          UIViews.render(:info, %{message: "You can't invite yourself in a party."})
        )

      {x, y} when x.group_id != nil and y.group_id != nil ->
        # i18n string 228 : Already in another party
        Socket.send(character.socket, UIViews.render(:infoi, %{i18n_vnum: 228}))

      {x, y} when x.group_id == y.group_id ->
        # i18n string 227 : Already in the requested party
        Socket.send(character.socket, UIViews.render(:infoi, %{i18n_vnum: 227}))

      _ ->
        callback.(character, target)
    end
  end
end
