defmodule ElvenPackets.Views.UIViews do
  @moduledoc """
  TODO: ElvenPackets.Views.UIViews
  """

  use ElvenGard.Network.View

  import ElvenPackets.View, only: [optional_param: 2, optional_param: 3, required_param: 2]

  alias ElvenCaching.Entity
  alias ElvenPackets.SubPackets.I18nSubPacket
  alias ElvenPackets.Server.UiPackets.{Cancel, Gb, Gold, Info, Scene, Smemoi, Smemoi2}

  ## Public API

  # FIXME: Move to ElvenGard.Network.View
  def render(name), do: render(name, [])

  @impl true
  def render(:cancel, args) do
    cancel_type = required_param(args, :cancel_type)
    entity = required_param(args, :entity)

    %Cancel{
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

    %Gb{
      action_type: action_type,
      bank_gold: character.bank_gold,
      gold: character.gold,
      bank_rank: bank_rank,
      bank_tax: bank_tax
    }
  end

  def render(:gold, args) do
    character = required_param(args, :character)

    %Gold{
      gold: character.gold,
      bank_gold: character.bank_gold
    }
  end

  def render(:info, args) do
    message = required_param(args, :message)

    %Info{message: message}
  end

  def render(:s_memoi, args) do
    i18n_key = required_param(args, :i18n_key)
    text_color = optional_param(args, :text_color)

    %Smemoi{
      text_color: text_color,
      i18n_packet: %I18nSubPacket{key: i18n_key}
    }
  end

  def render(:s_memoi2, args) do
    character = required_param(args, :character)
    i18n_key = required_param(args, :i18n_key)
    text_color = optional_param(args, :text_color)

    bank_gold =
      character.bank_gold
      |> Kernel./(1000)
      |> Kernel.trunc()
      |> ElvenCore.format_number()

    gold = ElvenCore.format_number(character.gold)

    %Smemoi2{
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

    %Scene{scene_id: scene_id, cancellable: cancellable}
  end
end
