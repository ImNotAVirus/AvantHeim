defmodule ElvenPackets.Server.UiPackets do
  @moduledoc """
  TODO: ElvenPackets.Server.UiPackets
  """

  use ElvenGard.Network.PacketSerializer

  import ElvenPackets.Enums.UIEnums

  alias ElvenPackets.Types.{NsInteger, NsString, NsEnum, NsBoolean}

  #######
  # Cancel skill
  # ---
  # Example: "cancel 1 50 -1"
  #######
  @serializable true
  defpacket "cancel", as: Cancel do
    field :cancel_type, NsEnum, values: cancel_type(:__enumerators__)
    field :entity_id, NsInteger
    field :unknown, NsInteger, default: -1
  end

  #######
  # Open bank window
  # ---
  # Example: "gb 1 50000 500 1 10000"
  #######
  @serializable true
  defpacket "gb", as: Gb do
    field :action_type, NsEnum, values: action_type(:__enumerators__)
    field :bank_gold, NsInteger
    field :gold, NsInteger
    field :bank_rank, NsInteger
    field :bank_tax, NsInteger
  end

  #######
  # Update gold display in inventory
  # ---
  # Example: "gold 50 500"
  #######
  @serializable true
  defpacket "gold", as: Gold do
    field :gold, NsInteger
    field :bank_gold, NsInteger
  end

  #######
  # Display message box
  # ---
  # Example: "gold 50 500"
  #######
  @serializable true
  defpacket "info", as: Info do
    field :message, NsString
  end

  # FIXME: Add i18n packets

  #######
  # Display act cinematic
  # ---
  # Example: "scene 1 0"
  #######
  @serializable true
  defpacket "scene", as: Scene do
    field :scene_id, NsInteger
    field :cancellable, NsBoolean
  end
end
