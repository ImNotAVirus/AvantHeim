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
            send_group_invitation(&join_character_group/2, character, target)

          group_request_type(:declined, :value) ->
            reject_invitation(character, target)

          _ ->
            :ok
        end

      {_, _} ->
        :ok
    end

    {:cont, socket}
  end

  # Private function

  defp reject_invitation(%Character{} = character, %Character{} = target) do
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
  end

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

  defp add_group(%Character{} = new_char) do
    case CachingService.write_character(new_char) do
      {:ok, new_char} ->
        # TODO: Send pinit & pst (Why the fuck pst is spammed on official each 1 second ?)
        {:ok, new_char}

      {:error, _} = x ->
        x
    end
  end

  defp join_character_group(%Character{} = character, %Character{} = target) do
    character_group = CachingService.get_characters_by_group_id(character.id)
    target_group = CachingService.get_characters_by_group_id(target.id)

    case {character_group, target_group} do
      {{:ok, c}, _} when length(c) > 1 ->
        new_char = %Character{target | group_id: character.id}
        add_group(new_char)

      {_, {:ok, t}} when length(t) > 1 ->
        new_char = %Character{target | group_id: target.id}
        add_group(new_char)

      {_, _} ->
        new_char = %Character{character | group_id: character.id}
        add_group(new_char)

        new_char = %Character{target | group_id: character.id}
        add_group(new_char)
    end
  end

  defp send_group_invitation(callback, %Character{} = character, %Character{} = target) do
    case {character, target, CachingService.get_characters_by_group_id(character.group_id)} do
      {x, y, _} when x.id == y.id ->
        Socket.send(
          character.socket,
          UIViews.render(:info, %{message: "You can't invite yourself in a party."})
        )

      {x, y, _} when x.group_id != nil and y.group_id != nil and x.group_id != y.group_id ->
        # i18n string 228 : Already in another party
        Socket.send(character.socket, UIViews.render(:infoi, %{i18n_vnum: 228}))

      {x, y, _} when x.group_id != nil and y.group_id != nil and x.group_id == y.group_id ->
        # i18n string 227 : Already in the requested party
        Socket.send(character.socket, UIViews.render(:infoi, %{i18n_vnum: 227}))

      {c, _, {:ok, players}} when c.group_id != nil and length(players) >= 3 ->
        # i18n string 230 : The party is already full
        Socket.send(character.socket, UIViews.render(:infoi, %{i18n_vnum: 230}))

      _ ->
        callback.(character, target)
    end
  end

  @spec broadcast_on_group(Character.t(), any, boolean) :: :ok
  defp broadcast_on_group(%Character{} = character, packet, including_self \\ true) do
    guards = if including_self, do: [], else: [{:!==, :group_id, character.group_id}]
    {:ok, players} = CachingService.get_characters_by_group_id(character.group_id, guards)
    Enum.each(players, &Socket.send(&1.socket, packet))
  end
end
