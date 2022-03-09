defmodule ElvenViews.UIViews do
  @moduledoc """
  TODO: Documentation
  """

  use ElvenViews

  alias ElvenCaching.Entity

  alias ElvenViews.SubPackets.I18nSubPacket

  alias ElvenViews.UIPackets.{
    CancelPacket,
    GbPacket,
    GoldPacket,
    InfoPacket,
    SMemoiPacket,
    SMemoi2Packet,
    ScenePacket
  }

  ## Public API

  @impl true
  def render(:cancel, args) do
    cancel_type = required_param(args, :cancel_type)
    entity = required_param(args, :entity)

    %CancelPacket{
      cancel_type: cancel_type,
      entity_id: Entity.id(entity)
    }
  end

  # TODO : Bank rank | tax | action_type
  def render(:gb, args) do
    character = required_param(args, :character)
    action_type = required_param(args, :action_type)
    bank_rank = required_param(args, :bank_rank)
    bank_tax = required_param(args, :bank_tax)

    %GbPacket{
      action_type: action_type,
      bank_gold: character.bank_gold,
      gold: character.gold,
      bank_rank: bank_rank,
      bank_tax: bank_tax
    }
  end

  def render(:gold, args) do
    character = required_param(args, :character)

    %GoldPacket{
      gold: character.gold,
      bank_gold: character.bank_gold
    }
  end

  def render(:info, args) do
    message = required_param(args, :message)

    %InfoPacket{message: message}
  end

  def render(:s_memoi, args) do
    i18n_key = required_param(args, :i18n_key)
    text_color = optional_param(args, :text_color)

    %SMemoiPacket{
      text_color: text_color,
      i18n_packet: %I18nSubPacket{key: i18n_key}
    }
  end

  def render(:s_memoi2, args) do
    character = required_param(args, :character)
    i18n_key = required_param(args, :i18n_key)
    text_color = optional_param(args, :text_color)

    bank_gold = ElvenCore.format_number(character.bank_gold)
    gold = ElvenCore.format_number(character.gold)

    %SMemoi2Packet{
      text_color: text_color,
      i18n_packet: %I18nSubPacket{
        key: i18n_key,
        args: [bank_gold, gold]
      }
    }
  end

  def render(:scene, args) do
    scene_id = required_param(args, :scene_id)
    cancellable = optional_param(args, :cancellable, true)

    %ScenePacket{scene_id: scene_id, cancellable: cancellable}
  end
end
