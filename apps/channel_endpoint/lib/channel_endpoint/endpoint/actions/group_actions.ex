defmodule ChannelEndpoint.Endpoint.GroupActions do
  @moduledoc """
  TODO: Documentation
  """

  alias Core.Socket
  alias CachingService.Player.Character
  alias ChannelEndpoint.Endpoint.{UIViews, PlayerViews, ChatViews}
  alias ChannelEndpoint.Endpoint.EntityInteractions

  import ChannelEndpoint.GroupRequestEnums, only: [group_request_type: 2]

  ## Packet handlers

  @spec delete_group(String.t(), map, Socket.t()) :: {:cont, Socket.t()}
  def delete_group("pleave", _args, %Socket{} = socket) do
    %{character_id: character_id} = socket.assigns
    {:ok, character} = CachingService.get_character_by_id(character_id)

    # msgi 0 478 0 0 0 0 0
    # broadcast_on_group(character, UIViews.render(), false)

    # TODO : if group master leave the group put someone else (in the group) as group master
    case CachingService.get_characters_by_group_id(character.group_id) do
      {:ok, players} ->
        case length(players) do
          2 ->
            Enum.each(players, fn player ->
              new_char = %Character{player | group_id: nil}
              write_character(new_char)
            end)

          3 ->
            new_char = %Character{character | group_id: nil}
            write_character(new_char)

          _ ->
            raise "Unsuported group length (Raid group ?)"
        end

      _ ->
        :ok
    end

    {:cont, socket}
  end

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

  defp write_character(%Character{} = new_char) do
    case CachingService.write_character(new_char) do
      {:ok, new_char} ->
        {:ok, new_char}

      {:error, _} = x ->
        x
    end
  end

  defp join_character_group(%Character{} = character, %Character{} = target) do
    character_group = CachingService.get_characters_by_group_id(character.group_id)
    target_group = CachingService.get_characters_by_group_id(target.group_id)

    case {character_group, target_group} do
      {{:ok, c}, {:ok, _}} when length(c) > 1 ->
        new_char = %Character{target | group_id: character.id}
        write_character(new_char)
        EntityInteractions.refresh_group_ui(new_char)

      {{:ok, _}, {:ok, t}} when length(t) > 1 ->
        new_char = %Character{character | group_id: target.id}
        write_character(new_char)
        EntityInteractions.refresh_group_ui(new_char)

      _ ->
        new_character = %Character{character | group_id: character.id}
        write_character(new_character)

        new_target = %Character{target | group_id: character.id}
        write_character(new_target)

        # i18n string 596 : You are the party master
        Socket.send(target.socket, UIViews.render(:infoi, %{i18n_vnum: 596}))

        # i18 string 477 : Joined a party
        Socket.send(character.socket, UIViews.render(:infoi, %{i18n_vnum: 477}))

        EntityInteractions.refresh_group_ui(new_target)
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
end
