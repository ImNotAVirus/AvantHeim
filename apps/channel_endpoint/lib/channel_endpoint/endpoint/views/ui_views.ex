defmodule ChannelEndpoint.Endpoint.UIViews do
  @moduledoc """
  TODO: Documentation
  """

  alias CachingService.Player.Character

  alias ChannelEndpoint.Endpoint.UIPackets.{
    Cancel,
    Info,
    Scene,
    Gold,
    Gb,
    SMemoi2,
    SMemoi,
    Dlgi2,
    Infoi2,
    Infoi,
    Msgi,
    Pinit,
    Pst,
    Pidx
  }

  ## Public API

  @spec render(atom, any) :: any
  def render(:info, %{message: message}), do: %Info{message: message}
  def render(:scene, %{scene_id: scene_id}), do: %Scene{scene_id: scene_id}

  def render(:cancel, %{type: type, entity: %Character{id: id}}) do
    %Cancel{type: type, entity_id: id}
  end

  def render(:gold, %Character{} = character) do
    %Gold{gold: character.gold, bank_gold: character.bank_gold}
  end

  # TODO : Bank rank | tax | action_type
  def render(:gb, %{
        entity: %Character{} = entity,
        action_type: action_type,
        bank_rank: bank_rank,
        bank_tax: bank_tax
      }) do
    %Gb{
      action_type: action_type,
      bank_gold: entity.bank_gold,
      gold: entity.gold,
      bank_rank: bank_rank,
      bank_tax: bank_tax
    }
  end

  def render(:s_memoi2, %{entity: %Character{} = entity, i18n_vnum: i18n_vnum} = attrs) do
    text_color = Map.get(attrs, :text_color, :white)

    %SMemoi2{
      text_color: text_color,
      i18n_vnum: i18n_vnum,
      bank_gold: entity.bank_gold,
      gold: entity.gold
    }
  end

  def render(:s_memoi, %{i18n_vnum: i18n_vnum} = attrs) do
    text_color = Map.get(attrs, :text_color, :white)

    %SMemoi{
      text_color: text_color,
      i18n_vnum: i18n_vnum
    }
  end

  def render(:dlgi2, %{
        packet_yes: packet_yes,
        packet_no: packet_no,
        i18n_vnum: i18n_vnum,
        params_count: params_count,
        name: name
      }) do
    %Dlgi2{
      packet_yes: packet_yes,
      packet_no: packet_no,
      i18n_vnum: i18n_vnum,
      params_count: params_count,
      name: name
    }
  end

  def render(:infoi2, %{
        i18n_vnum: i18n_vnum,
        params_count: params_count,
        entity: %Character{name: name}
      }) do
    %Infoi2{
      i18n_vnum: i18n_vnum,
      params_count: params_count,
      name: name
    }
  end

  def render(:infoi, %{i18n_vnum: i18n_vnum}), do: %Infoi{i18n_vnum: i18n_vnum}

  def render(:msgi, %{message_type: message_type, i18n_vnum: i18n_vnum}) do
    %Msgi{
      message_type: message_type,
      i18n_vnum: i18n_vnum
    }
  end


  def render(:pinit, %{group_size: group_size, members: members}) do
    %Pinit{
      group_size: group_size,
      members: members
    }
  end

  def render(:pst, %Character{} = character) do
    %Pst{
      entity_type: :character,
      entity_id: character.id,
      group_order: 0,
      hp_left: FakeData.hp(character_id: character.id),
      mp_left: FakeData.mp(character_id: character.id),
      hp_load: FakeData.hp_max(character_id: character.id),
      mp_load: FakeData.mp_max(character_id: character.id),
      race: 0,
      gender: character.gender,
      morph: FakeData.morph(character_id: character.id),
      # TODO
      buff_ids: []
    }
  end

  def render(:pidx, %{entity: %Character{group_id: group_id}, sub_packet: sub_packet}) do
    %Pidx{
      group_id: group_id,
      sub_packet: sub_packet
    }
  end
end
