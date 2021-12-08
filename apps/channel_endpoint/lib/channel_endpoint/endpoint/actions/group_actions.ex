defmodule ChannelEndpoint.Endpoint.GroupActions do
  @moduledoc """
  TODO: Documentation
  """

  alias Core.Socket
  alias CachingService.Player.Character
  alias ChannelEndpoint.Endpoint.{UIViews, PlayerViews, ChatViews}
  alias ChannelEndpoint.Endpoint.EntityInteractions

  import ChannelEndpoint.GroupRequestEnums, only: [group_request_type: 2]

  @max_group_players 3

  ## Packet handlers

  @spec delete_group(String.t(), map, Socket.t()) :: {:cont, Socket.t()}
  def delete_group("pbreak", _args, %Socket{} = socket) do
    %{character_id: character_id} = socket.assigns
    {:ok, character} = CachingService.get_character_by_id(character_id)

    case CachingService.get_characters_by_group_id(character.group_id) do
      {:ok, players} ->
        case length(players) do
          2 ->
            Enum.each(players, fn player ->
              new_char = %Character{player | group_id: -1}
              write_character(new_char)
              Socket.send(new_char.socket, UIViews.render(:pinit_empty_group, %{unknow: 0}))
              EntityInteractions.see_player_not_in_group_anymore(new_char)
              # Party disbanded
              Socket.send(
                new_char.socket,
                UIViews.render(:msgi, %{message_type: :whisper, i18n_vnum: 478})
              )
            end)

          @max_group_players ->
            define_new_group_owner(character)
            EntityInteractions.see_player_not_in_group_anymore(character)

          _ ->
            raise "Unsuported group length (Raid group ?)"
        end

      _ ->
        :ok
    end

    {:cont, socket}
  end

  @spec define_new_group_owner(Character.t()) :: any | :ignore
  def define_new_group_owner(%Character{} = character) do
    case {character.group_id,
          CachingService.get_characters_by_group_id(character.group_id, [
            {:!==, :id, character.id}
          ])} do
      {x, {:ok, players}} when x == character.id ->
        new_owner = Enum.at(players, 0)

        Enum.each(players, fn player ->
          new_char = %Character{player | group_id: new_owner.id}
          write_character(new_char)
        end)

        remove_group(character)
        EntityInteractions.refresh_group_list(new_owner, players)
        # You are now the party master
        Socket.send(new_owner.socket, UIViews.render(:infoi, %{i18n_vnum: 596}))

      {_, {:ok, players}} ->
        remove_group(character)
        EntityInteractions.refresh_group_list(character, players)
        Socket.send(character.socket, UIViews.render(:pinit_empty_group, %{unknow: 0}))
    end
  end

  @spec create_group(String.t(), map, Socket.t()) :: {:cont, Socket.t()}
  def create_group(
        "pjoin",
        %{request_type: request_type, entity_id: entity_id},
        %Socket{} = socket
      ) do
    %{character_id: character_id} = socket.assigns
    {:ok, character} = CachingService.get_character_by_id(character_id)

    cond do
      character.last_group_req_timestamp !== -1 and
        character.last_group_req_timestamp > :os.system_time(:seconds) and
          character.last_group_req_timestamp + 5 < :os.system_time(:seconds) == true ->
        Socket.send(
          character.socket,
          UIViews.render(:info, %{message: "You send out the invitations too fast !"})
        )

      true ->
        new_char = %Character{character | last_group_req_timestamp: :os.system_time(:seconds)}
        write_character(new_char)

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
    end

    {:cont, socket}
  end

  @spec group_say(String.t(), map, Socket.t()) :: {:cont, Socket.t()}
  def group_say(";", %{message: message}, %Socket{} = socket) do
    %{character_id: character_id} = socket.assigns
    {:ok, character} = CachingService.get_character_by_id(character_id)

    EntityInteractions.say_in_group(character, message)

    {:cont, socket}
  end

  # Private function

  defp remove_group(%Character{} = character) do
    new_char = %Character{character | group_id: -1}
    write_character(new_char)
  end

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
        new_char = %Character{
          target
          | group_id: character.group_id
        }

        write_character(new_char)
        EntityInteractions.refresh_group_ui(new_char)

      {{:ok, _}, {:ok, t}} when length(t) > 1 ->
        new_char = %Character{
          character
          | group_id: target.group_id
        }

        write_character(new_char)
        EntityInteractions.refresh_group_ui(new_char)

      _ ->
        new_character = %Character{character | group_id: target.id}

        write_character(new_character)

        new_target = %Character{target | group_id: target.id}
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

      {x, y, _} when x.group_id != -1 and y.group_id != -1 and x.group_id != y.group_id ->
        # i18n string 228 : Already in another party
        Socket.send(character.socket, UIViews.render(:infoi, %{i18n_vnum: 228}))

      {x, y, _} when x.group_id != -1 and y.group_id != -1 and x.group_id == y.group_id ->
        # i18n string 227 : Already in the requested party
        Socket.send(character.socket, UIViews.render(:infoi, %{i18n_vnum: 227}))

      {c, _, {:ok, players}} when c.group_id != -1 and length(players) >= @max_group_players ->
        # i18n string 230 : The party is already full
        Socket.send(character.socket, UIViews.render(:infoi, %{i18n_vnum: 230}))

      _ ->
        callback.(character, target)
    end
  end
end
