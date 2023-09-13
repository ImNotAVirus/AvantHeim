defmodule ElvenPackets.Views.UIViews do
  @moduledoc """
  TODO: ElvenPackets.Views.UIViews
  """

  use ElvenGard.Network.View

  import ElvenPackets.View, only: [optional_param: 2, optional_param: 3, required_param: 2]

  alias GameService.PlayerBundle
  alias ElvenCaching.Entity
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

  # TODO : Bank rank | tax | action_type
  def render(:gb, args) do
    entity = required_param(args, :entity)
    action_type = required_param(args, :action_type)

    if entity.__struct__ != PlayerBundle do
      raise ArgumentError, "gb can only be called on players, got: #{inspect(entity)}"
    end

    %Gb{
      action_type: action_type,
      gold: PlayerBundle.gold(entity),
      bank_gold: PlayerBundle.bank_gold(entity),
      bank_rank: PlayerBundle.bank_rank(entity),
      bank_tax: PlayerBundle.bank_tax(entity)
    }
  end

  def render(:gold, args) do
    entity = required_param(args, :entity)

    if entity.__struct__ != PlayerBundle do
      raise ArgumentError, "gold can only be called on players, got: #{inspect(entity)}"
    end

    %Gold{
      gold: PlayerBundle.gold(entity),
      bank_gold: PlayerBundle.bank_gold(entity)
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
    entity = required_param(args, :entity)
    i18n_key = required_param(args, :i18n_key)
    text_color = optional_param(args, :text_color)

    if entity.__struct__ != PlayerBundle do
      raise ArgumentError, "s_memoi2 can only be called on players, got: #{inspect(entity)}"
    end

    bank_gold =
      PlayerBundle.bank_gold(entity)
      |> Kernel./(1000)
      |> Kernel.trunc()
      |> ElvenPackets.format_number()

    gold = ElvenPackets.format_number(PlayerBundle.gold(entity))

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
