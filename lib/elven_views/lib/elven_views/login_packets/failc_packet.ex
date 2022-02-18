defmodule ElvenViews.LoginPackets.FailcPacket do
  @moduledoc """
  TODO: Documentation.
  """

  use ElvenViews.SerializablePacket

  import ElvenViews.LoginPackets.FailcEnums, only: [error: 1]

  ## Packet definition

  defpacket "failc" do
    field :error, :enum, default: :generic, values: error(:__enumerators__)
  end
end
