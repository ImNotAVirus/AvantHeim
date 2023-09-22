defmodule ChannelService.PacketSchemas do
  @moduledoc """
  TODO: Documentation
  """

  use ElvenCore.CommandSchema

  ## Commands

  defcommand "speed", ChannelService.SpeedCommand
  defcommand "name", ChannelService.NameCommand
  defcommand "effect", ChannelService.EffectCommand
  defcommand "gold", ChannelService.GoldCommand
  defcommand "bank", ChannelService.BankCommand
end
