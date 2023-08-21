defmodule ElvenViews.PlayerPackets.FsPacket do
  @moduledoc """
  TODO: Documentation.
  """

  use ElvenViews.SerializablePacket

  import ElvenEnums.PlayerEnums, only: [faction: 1]

  ## Packet definition

  defpacket "fs" do
    field :faction, :enum, values: faction(:__enumerators__)
  end
end
