defmodule ChannelService.Endpoint do
  @moduledoc """
  Documentation for ChannelService.Endpoint
  """

  use ElvenGard.Network.Endpoint, otp_app: :channel_service

  require Logger

  ## Callbacks

  @impl true
  def handle_start(config) do
    host = get_in(config, [:transport_opts, :socket_opts, :ip])
    port = get_in(config, [:transport_opts, :socket_opts, :port])
    Logger.info("ChannelService started on #{:inet.ntoa(host)}:#{port}")
  end
end
