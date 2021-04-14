defmodule LoginEndpoint.Endpoint.Protocol do
  @moduledoc false

  use GenServer

  require Logger

  @behaviour :ranch_protocol

  @timeout 5_000

  ## Ranch Protocol behaviour

  @impl true
  def start_link(ref, transport, opts) do
    pid = :proc_lib.spawn_link(__MODULE__, :init, [{ref, transport, opts}])
    {:ok, pid}
  end

  ## GenServer behaviour

  @impl true
  def init({ref, transport, _opts}) do
    {:ok, transport_pid} = :ranch.handshake(ref)

    socket = %{
      id: Core.UUID.uuid4(),
      transport_pid: transport_pid,
      transport: transport
    }

    Logger.info("New connection: #{socket.id}")

    transport.setopts(transport_pid, active: :once)
    :gen_server.enter_loop(__MODULE__, [], socket, @timeout)
  end

  @impl true
  def handle_info({:tcp, transport_pid, message}, socket) do
    %{id: id, transport_pid: ^transport_pid, transport: transport} = socket

    Logger.debug("New message from #{id} (len: #{byte_size(message)})")

    transport.setopts(transport_pid, active: :once)
    {:noreply, socket}
  end

  def handle_info({:tcp_closed, transport_pid}, socket) do
    %{id: id, transport_pid: ^transport_pid} = socket
    Logger.info("#{id} is now disconnected")
    {:stop, :normal, socket}
  end

  def handle_info(:timeout, socket) do
    %{id: id, transport_pid: transport_pid, transport: transport} = socket
    Logger.error("An error occured with client #{id}: :timeout")
    transport.shutdown(transport_pid, :read_write)
    {:stop, {:shutdown, :timeout}, socket}
  end
end
