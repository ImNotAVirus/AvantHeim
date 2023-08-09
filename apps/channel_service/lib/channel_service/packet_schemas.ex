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

  ## Area packets

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
