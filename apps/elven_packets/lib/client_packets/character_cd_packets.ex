defmodule ElvenPackets.Client.CharacterCdPackets do
  @moduledoc """
  TODO: ElvenPackets.Client.LoginPackets
  """

  use ElvenGard.Network.PacketSerializer

  alias ElvenPackets.Types.{NsInteger, NsString}

  #######
  # Ask for a character creation
  # ---
  # Example: "Char_NEW TestChar 0 1 1 2"
  #######
  @deserializable true
  defpacket "Char_NEW" do
    field :name, NsString
    field :slot, NsInteger
    field :gender, NsInteger, desc: "Enum: GenderType"
    field :hair_style, NsInteger, desc: "Enum: HairStyle"
    field :hair_color, NsInteger, desc: "Enum: HairColor"
  end

  #######
  # Ask for a character suppression
  # ---
  # Example: "Char_DEL 3 password"
  #######
  @deserializable true
  packet "Char_DEL" do
    field :slot, NsInteger
    field :password, NsString
  end
end
