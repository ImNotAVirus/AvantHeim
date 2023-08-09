defmodule ChannelService.PacketSchemas do
  @moduledoc """
  TODO: Documentation
  """

  use ElvenCore.{CommandSchema, PacketSchema}

  alias ChannelService.Endpoint.{
    LobbyActions,
    GameActions,
    MapActions,
    ChatActions,
    UIActions
  }

  import ElvenViews.UIPackets.GuriEnums, only: [guri_type: 2]

  ## Ignore some packets

  ignore_packet "0"
  ignore_packet "c_close"
  ignore_packet "f_stash_end"
  ignore_packet "lbs"

  ## Lobby packets

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

  #######
  # Handle packet sended by the client when a player is pressing his arrow keyboard
  # ---
  # Example: "dir 1 2 3"
  #######
  packet "dir" do
    field :dir, :integer
    field :entity_type, :integer
    field :entity_id, :integer

    resolve MapActions, :dir
  end

  ## Commands

  defcommand "speed", ChannelService.Endpoint.SpeedCommand
  defcommand "name", ChannelService.Endpoint.NameCommand
  defcommand "effect", ChannelService.Endpoint.EffectCommand
  defcommand "gold", ChannelService.Endpoint.GoldCommand
  defcommand "bank", ChannelService.Endpoint.BankCommand
end
