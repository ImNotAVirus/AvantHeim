defmodule ChannelEndpoint.PacketSchemas do
  @moduledoc """
  TODO: Documentation
  """

  use Core.{CommandSchema, PacketSchema}

  alias ChannelEndpoint.Endpoint.{
    LobbyActions,
    GameActions,
    MapActions,
    ChatActions,
    UIActions
  }

  import ChannelEndpoint.Endpoint.UIPackets.Guri, only: [guri_type: 2]

  ## Ignore some packets

  ignore_packet "0"
  ignore_packet "c_close"
  ignore_packet "f_stash_end"
  ignore_packet "lbs"

  ## Lobby packets

  #######
  # Ask for a character creation
  # ---
  # Example: "Char_NEW TestChar 0 1 1 2"
  #######
  packet "Char_NEW" do
    field :name, :string
    field :slot, :integer
    field :gender, :integer, desc: "Enum: GenderType"
    field :hair_style, :integer, desc: "Enum: HairStyle"
    field :hair_color, :integer, desc: "Enum: HairColor"

    resolve LobbyActions, :create_character
  end

  #######
  # Ask for a character suppression
  # ---
  # Example: "Char_DEL 3 password"
  #######
  packet "Char_DEL" do
    field :slot, :integer
    field :password, :string

    resolve LobbyActions, :delete_character
  end

  #######
  # Select a character
  # ---
  # Example: "select 2"
  #######
  packet "select" do
    field :slot, :integer

    resolve LobbyActions, :select_character
  end

  #######
  # Enter in game
  # ---
  # Example: "game_start"
  #######
  packet "game_start" do
    resolve GameActions, :game_start
  end

  ## Area packets

  packet "walk" do
    field :pos_x, :integer
    field :pos_y, :integer
    field :checksum, :integer
    field :speed, :integer

    resolve MapActions, :walk
  end

  #######
  # Handle message coming from the game chat
  # ---
  # Example: "say DarkyZ"
  #######
  packet "say" do
    field :message, :string, fill: true

    resolve ChatActions, :player_general_chat
  end

  #######
  # Request stats info about mobs or mates or character
  # ---
  # Example: "ncif 1 123"
  #######
  packet "ncif" do
    field :entity_type, :integer
    field :entity_id, :integer

    resolve MapActions, :ncif
  end

  #######
  # Handle emote effect asked by the client and return the specified effect packet
  # ---
  # Example: "guri 10 1 20 5099"
  #######
  packet "guri" do
    field :type, :integer, using: guri_type(:emoji, :value)
    field :entity_type, :integer
    field :entity_id, :integer
    field :guri_data, :integer

    resolve UIActions, :show_emoji
  end

  ## Commands

  defcommand "speed", ChannelEndpoint.Endpoint.SpeedCommand
  defcommand "name", ChannelEndpoint.Endpoint.NameCommand
  defcommand "effect", ChannelEndpoint.Endpoint.EffectCommand
  defcommand "gold", ChannelEndpoint.Endpoint.GoldCommand
  defcommand "bank", ChannelEndpoint.Endpoint.BankCommand
end
