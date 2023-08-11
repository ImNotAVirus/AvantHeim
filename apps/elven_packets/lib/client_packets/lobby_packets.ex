defmodule ElvenPackets.Client.LobbyPackets do
  @moduledoc """
  TODO: ElvenPackets.Client.LobbyPackets
  """

  use ElvenGard.Network.PacketSerializer

  import ElvenPackets.Enums.LobbyEnums, only: [hair_color: 1, hair_style: 1, gender: 1]

  alias ElvenPackets.Types.{NsInteger, NsString, NsEnum}

  #######
  # Ask for a character creation
  # ---
  # Example: "Char_NEW TestChar 0 1 1 2"
  #######
  @deserializable true
  defpacket "Char_NEW", as: CharNEW do
    field :name, NsString
    field :slot, NsInteger
    field :gender, NsEnum, values: gender(:__enumerators__)
    field :hair_style, NsEnum, values: hair_style(:__enumerators__)
    field :hair_color, NsEnum, values: hair_color(:__enumerators__)
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
