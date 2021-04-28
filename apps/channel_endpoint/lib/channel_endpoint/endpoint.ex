defmodule ChannelEndpoint.Endpoint do
  @moduledoc """
  TODO: Documentation
  """

  require Logger

  ## Public API

  @doc """
  Returns the child specification to start the endpoint
  under a supervision tree.
  """
  def child_spec(_opts) do
    config = fetch_config!()

    log_starting(config)

    :ranch.child_spec(
      {__MODULE__, config[:listener_name]},
      config[:transport],
      config[:transport_opts],
      config[:protocol],
      config[:protocol_opts]
    )
  end

  ## Private functions

  defp fetch_config!() do
    Application.fetch_env!(:channel_endpoint, __MODULE__)
  end

  defp log_starting(config) do
    host = get_in(config, [:transport_opts, :ip])
    port = get_in(config, [:transport_opts, :port])
    Logger.info("ChannelEndpoint started on #{:inet.ntoa(host)}:#{port}")
  end
end
