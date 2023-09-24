defmodule ElvenPackets.Views.UIViews do
  @moduledoc """
  TODO: ElvenPackets.Views.UIViews
  """

  use ElvenGard.Network.View

  import ElvenPackets.View, only: [optional_param: 2, optional_param: 3, required_param: 2]

  alias ElvenPackets.SubPackets.I18nSubPacket
  alias ElvenPackets.Server.UiPackets.{Cancel, Gb, Gold, Info, Scene, SMemoi, SMemoi2}

  ## Public API

  # FIXME: Move to ElvenGard.Network.View
  def render(name), do: render(name, %{})

  @impl true
  def render(:cancel, args) do
    entity = required_param(args, :entity)
    cancel_type = required_param(args, :cancel_type)

    %Cancel{
      cancel_type: cancel_type,
      entity_id: GameService.entity_id(entity)
    }
  end

  def render(:gb, args) do
    %Gb{
      action_type: required_param(args, :action_type),
      gold: required_param(args, :gold),
      bank_gold: required_param(args, :bank_gold),
      bank_rank: required_param(args, :bank_rank),
      bank_tax: required_param(args, :bank_tax)
    }
  end

  def render(:gold, args) do
    %Gold{
      gold: required_param(args, :gold),
      bank_gold: required_param(args, :bank_gold)
    }
  end

  def render(:info, args) do
    message = required_param(args, :message)
    %Info{message: message}
  end

  def render(:s_memoi, args) do
    i18n_key = required_param(args, :i18n_key)
    text_color = optional_param(args, :text_color)

    %SMemoi{
      text_color: text_color,
      i18n_packet: %I18nSubPacket{key: i18n_key}
    }
  end

  def render(:s_memoi2, args) do
    i18n_key = required_param(args, :i18n_key)
    text_color = optional_param(args, :text_color)

    bank_gold =
      args
      |> required_param(:bank_gold)
      |> Kernel./(1000)
      |> Kernel.trunc()
      |> ElvenPackets.format_number()

    gold =
      args
      |> required_param(:gold)
      |> ElvenPackets.format_number()

    %SMemoi2{
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
