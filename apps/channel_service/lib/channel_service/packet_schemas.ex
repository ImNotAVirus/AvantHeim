defmodule ChannelService.PacketSchemas do
  @moduledoc """
  TODO: Documentation
  """

  use ElvenCore.{CommandSchema, PacketSchema}

  ## Ignore some packets

  ignore_packet "0"
  ignore_packet "c_close"
  ignore_packet "f_stash_end"
  ignore_packet "lbs"

  ## Commands

  defcommand "speed", ChannelService.SpeedCommand
  defcommand "name", ChannelService.NameCommand
  defcommand "effect", ChannelService.EffectCommand
  defcommand "gold", ChannelService.GoldCommand
  defcommand "bank", ChannelService.BankCommand
end
