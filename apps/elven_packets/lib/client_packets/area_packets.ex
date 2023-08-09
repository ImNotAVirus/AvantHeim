defmodule ElvenPackets.Client.AreaPackets do
  @moduledoc """
  TODO: ElvenPackets.Client.AreaPackets
  """

  use ElvenGard.Network.PacketSerializer

  alias ElvenPackets.Types.{NsInteger, NsString}

  import ElvenViews.UIPackets.GuriEnums, only: [guri_type: 2]

  #######
  # Walk around the map
  # ---
  # Example: "walk 3 1 313 25"
  #######
  @deserializable true
  defpacket "walk", as: Walk do
    field :pos_x, NsInteger
    field :pos_y, NsInteger
    field :checksum, NsInteger
    field :speed, NsInteger
  end

  #######
  # Handle message coming from the game chat
  # ---
  # Example: "say DarkyZ"
  #######
  @deserializable true
  defpacket "say", as: Say do
    field :message, NsString, fill: true
  end

  #######
  # Request stats info about mobs or mates or character
  # ---
  # Example: "ncif 1 123"
  #######
  @deserializable true
  defpacket "ncif", as: Ncif do
    field :entity_type, NsInteger
    field :entity_id, NsInteger
  end

  #######
  # Display emoji, do dance ect
  # ---
  # Example: "guri 10 2 1 0"
  #######
  @deserializable true
  defpacket "guri", as: Guri do
    field :type, NsInteger, using: guri_type(:emoji, :value)
    field :entity_type, NsInteger
    field :entity_id, NsInteger
    field :guri_data, NsInteger
  end

  #######
  # Handle packet sended by the client when a player is pressing his arrow keyboard
  # ---
  # Example: "dir 1 2 3"
  #######
  @deserializable true
  defpacket "dir", as: Dir do
    field :dir, NsInteger
    field :entity_type, NsInteger
    field :entity_id, NsInteger
  end
end
