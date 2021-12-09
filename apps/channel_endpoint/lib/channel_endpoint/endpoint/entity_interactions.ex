defmodule ChannelEndpoint.Endpoint.EntityInteractions do
  @moduledoc """
  TODO: Break theses functions into another modules
  """

  alias Core.Socket
  alias CachingService.Position
  alias CachingService.Player.Character
  alias DatabaseService.EntityEnums

  alias ChannelEndpoint.Endpoint.{
    EntityViews,
    MapViews,
    PlayerViews,
    VisibilityViews,
    UIViews,
    ChatViews
  }

  @spec map_enter(Character.t()) :: :ok
  def map_enter(%Character{} = character) do
    ## Self packets
    Socket.send(character.socket, PlayerViews.render(:c_info, character))
    Socket.send(character.socket, EntityViews.render(:c_mode, character))
    Socket.send(character.socket, PlayerViews.render(:lev, character))
    Socket.send(character.socket, PlayerViews.render(:stat, character))
    Socket.send(character.socket, MapViews.render(:at, character))
    Socket.send(character.socket, MapViews.render(:c_map, character))
    # TODO: Socket.send(character.socket, PlayerViews.render(:sc, character))
    Socket.send(character.socket, EntityViews.render(:char_sc, character))
    Socket.send(character.socket, EntityViews.render(:cond, character))
    Socket.send(character.socket, UIViews.render(:pinit_empty_group, %{unknow: 0}))

    if character.group_id !== -1 do
      refresh_group_ui(character)
    end

    ## Other players packets
    %Position{map_id: map_id} = Character.get_position(character)

    {:ok, players} = CachingService.get_characters_by_map_id(map_id, [{:!==, :id, character.id}])
    Enum.each(players, &send_visibility_packets(character, &1))
  end

  @spec add_member_to_list(Character.t()) :: SubGroupMember.t()
  def add_member_to_list(%Character{} = character) do
    UIViews.render(:pinit_sub_member, character)
  end

  @spec get_group_member_list(List.t()) :: list
  def get_group_member_list(players) do
    Enum.flat_map(players, fn player ->
      group_member = add_member_to_list(player)
      [group_member]
    end)
  end

  @spec refresh_group_list(Character.t(), List.t()) :: :ok
  def refresh_group_list(%Character{} = character, players) do
    members_list = get_group_member_list(players)

    broadcast_on_group(
      character,
      UIViews.render(:pinit, %{group_size: length(players), members: members_list})
    )
  end

  @spec refresh_group_ui(Character.t()) :: {:ok, new_char :: Character.t()} | {:error, atom}
  def refresh_group_ui(%Character{} = character) do
    case CachingService.get_characters_by_group_id(character.group_id) do
      {:ok, players} ->
        refresh_group_list(character, players)

        Enum.each(players, fn player ->
          broadcast_on_group(
            player,
            UIViews.render(:pst, player),
            false
          )
        end)

        see_player_in_group(character)

      _ ->
        :ok
    end
  end

  @spec get_pidx_sub_packet(List.t()) :: list
  def get_pidx_sub_packet(players) do
    Enum.flat_map(players, fn player ->
      subpacket =
        UIViews.render(:pidx_sub_member, %{is_grouped: player.group_id !== -1, entity: player})

      [subpacket]
    end)
  end

  @spec see_player_in_group(Character.t()) :: any
  def see_player_in_group(%Character{} = character) do
    case CachingService.get_characters_by_group_id(character.group_id) do
      {:ok, players} ->
        sub_packet = get_pidx_sub_packet(players)

        broadcast_on_map(
          character,
          UIViews.render(:pidx, %{group_id: character.group_id, characters: sub_packet})
        )

      _ ->
        :ignore
    end
  end

  @spec see_player_not_in_group_anymore(Character.t()) :: :ok
  def see_player_not_in_group_anymore(%Character{} = character) do
    subpacket = UIViews.render(:pidx_sub_member, %{is_grouped: true, entity: character})

    broadcast_on_map(
      character,
      UIViews.render(:pidx, %{group_id: -1, characters: subpacket})
    )
  end

  @spec set_dir(Character.t(), EntityEnums.direction_type_keys()) ::
          {:ok, new_char :: Character.t()} | {:error, atom}
  def set_dir(%Character{} = character, new_dir) do
    new_char = %Character{character | direction: new_dir}

    case CachingService.write_character(new_char) do
      {:ok, new_char} ->
        broadcast_on_map(new_char, EntityViews.render(:dir, new_char), false)
        {:ok, new_char}

      {:error, _} = x ->
        x
    end
  end

  @spec say_to_map(Character.t(), String.t()) :: :ok
  def say_to_map(%Character{} = character, message) do
    broadcast_on_map(
      character,
      ChatViews.render(:say, %{entity: character, message: message}),
      false
    )
  end

  @spec say_in_group(Character.t(), String.t()) :: :ok
  def say_in_group(%Character{} = character, message) do
    broadcast_on_group(
      character,
      ChatViews.render(:spk, %{
        entity: character,
        speak_type: :group,
        message: message
      })
    )
  end

  @spec open_bank_window(Character.t()) :: :ok
  def open_bank_window(%Character{} = character) do
    # Open an empty bank widget
    Socket.send(
      character.socket,
      UIViews.render(:gb, %{
        entity: character,
        action_type: :open_from_savings_book,
        bank_rank: 1,
        bank_tax: 0
      })
    )

    # Text: Balance: %s Golds; Carrying: %s Gold
    Socket.send(
      character.socket,
      UIViews.render(:s_memoi2, %{entity: character, i18n_vnum: 2345})
    )

    # Text: We'll do our best. Thank you for using the Cuarry Bank.
    Socket.send(
      character.socket,
      UIViews.render(:s_memoi, %{i18n_vnum: 2353})
    )
  end

  # TODO : Improve that to support pnj | mobs | mates
  @spec show_effect(Character.t(), pos_integer) :: :ok
  def show_effect(%Character{} = character, effect_value) do
    broadcast_on_map(
      character,
      EntityViews.render(:eff, %{entity: character, value: effect_value})
    )
  end

  @spec set_player_golds(Character.t(), 0..2_000_000_000) ::
          {:ok, new_char :: Character.t()} | {:error, atom}
  def set_player_golds(%Character{} = character, new_player_gold) do
    norms_gold = normalize_golds(new_player_gold, 2_000_000_000)
    new_char = %Character{character | gold: norms_gold}
    send_gold_ui(new_char)
  end

  @spec set_bank_golds(Character.t(), 0..5_000_000_000) ::
          {:ok, new_char :: Character.t()} | {:error, atom}
  def set_bank_golds(%Character{} = character, new_bank_gold) do
    norms_gold = normalize_golds(new_bank_gold, 5_000_000_000)
    new_char = %Character{character | bank_gold: norms_gold}
    send_gold_ui(new_char)
  end

  @spec set_speed(Character.t(), 0..59) :: {:ok, new_char :: Character.t()} | {:error, atom}
  def set_speed(%Character{} = character, new_speed) do
    new_char = %Character{character | speed: new_speed}

    case CachingService.write_character(new_char) do
      {:ok, new_char} ->
        broadcast_on_map(new_char, EntityViews.render(:cond, new_char))
        {:ok, new_char}

      {:error, _} = x ->
        x
    end
  end

  @spec move(Character.t(), non_neg_integer, non_neg_integer) ::
          {:ok, new_char :: Character.t()} | {:error, atom}
  def move(%Character{} = character, new_x, new_y) do
    new_char = %Character{character | map_x: new_x, map_y: new_y}

    case CachingService.write_character(new_char) do
      {:ok, new_char} ->
        broadcast_on_map(new_char, MapViews.render(:mv, new_char), false)
        {:ok, new_char}

      {:error, _} = x ->
        x
    end
  end

  ## Private functions

  @spec normalize_golds(non_neg_integer, non_neg_integer) :: non_neg_integer
  defp normalize_golds(golds, max_val) do
    case golds do
      g when g < 0 -> 0
      g when g > max_val -> max_val
      g -> g
    end
  end

  @spec send_gold_ui(Character.t()) :: {:ok, Character.t()} | {:error, any}
  defp send_gold_ui(%Character{} = character) do
    case CachingService.write_character(character) do
      {:ok, character} ->
        Socket.send(character.socket, UIViews.render(:gold, character))
        {:ok, character}

      {:error, _} = x ->
        x
    end
  end

  @spec broadcast_on_map(Character.t(), any, boolean) :: :ok
  defp broadcast_on_map(%Character{} = character, packet, including_self \\ true) do
    guards = if including_self, do: [], else: [{:!==, :id, character.id}]
    %Position{map_id: map_id} = Character.get_position(character)
    {:ok, players} = CachingService.get_characters_by_map_id(map_id, guards)
    Enum.each(players, &Socket.send(&1.socket, packet))
  end

  @spec send_visibility_packets(Character.t(), Character.t()) :: :ok
  defp send_visibility_packets(self, character) do
    Socket.send(self.socket, VisibilityViews.render(:in, character))
    Socket.send(character.socket, VisibilityViews.render(:in, self))
    Socket.send(self.socket, EntityViews.render(:c_mode, character))
    Socket.send(character.socket, EntityViews.render(:c_mode, self))
  end

  @spec broadcast_on_group(Character.t(), any, boolean) :: :ok
  defp broadcast_on_group(%Character{} = character, packet, including_self \\ true) do
    guards = if including_self, do: [], else: [{:!==, :id, character.id}]
    {:ok, players} = CachingService.get_characters_by_group_id(character.group_id, guards)
    Enum.each(players, &Socket.send(&1.socket, packet))
  end
end
