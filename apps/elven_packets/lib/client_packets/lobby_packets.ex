defmodule ElvenPackets.Client.LobbyPackets do
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
  defpacket "Char_NEW", as: CharNEW do
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
  defpacket "Char_DEL", as: CharDEL do
    field :slot, NsInteger
    field :password, NsString
  end

  #######
  # Select a character
  # ---
  # Example: "select 2"
  #######
  @deserializable true
  defpacket "select", as: Select do
    field :slot, NsInteger
  end

  #######
  # Enter in game
  # ---
  # Example: "game_start"
  #######
  @deserializable true
  defpacket "game_start", as: GameStart
end
