defmodule LoginService.Endpoint do
  @moduledoc """
  Documentation for LoginService.Endpoint
  """

  use ElvenGard.Network.Endpoint, otp_app: :login_service

  require Logger

  ## Callbacks

  @impl true
  def handle_start(config) do
    host = get_in(config, [:transport_opts, :socket_opts, :ip])
    port = get_in(config, [:transport_opts, :socket_opts, :port])
    Logger.info("LoginService started on #{:inet.ntoa(host)}:#{port}")
  end
end
