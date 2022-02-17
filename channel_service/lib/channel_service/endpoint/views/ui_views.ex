defmodule ChannelService.Endpoint.UIViews do
  @moduledoc """
  TODO: Documentation
  """

  alias ElvenCaching.Entity.Character
  alias ChannelService.Endpoint.UIPackets.{Cancel, Info, Scene, Gold, Gb, SMemoi2, SMemoi}

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
end