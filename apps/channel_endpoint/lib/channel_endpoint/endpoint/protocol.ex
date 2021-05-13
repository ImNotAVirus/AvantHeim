defmodule ChannelEndpoint.Endpoint.Protocol do
  @moduledoc false

  use GenServer

  require Logger

  alias Core.Socket

  @behaviour :ranch_protocol

  @handshake_timeout 3_000
  @timeout 70_000
  @separator " "

  @packet_encoder ChannelEndpoint.Endpoint.Cryptography
  @packet_schemas Application.fetch_env!(:channel_endpoint, :packet_schemas)

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
    {:ok, {address, port}} = :inet.peername(transport_pid)

    socket = Socket.new(transport, transport_pid, @packet_encoder)

    Logger.info("New connection: #{socket.id} (#{:inet.ntoa(address)}:#{port})")

    :gen_server.enter_loop(__MODULE__, [], socket, {:continue, :client_handshake})
  end

  @impl true
  def handle_continue(:client_handshake, socket) do
    new_socket = recv_session_key(socket)
    {session_id, password} = recv_credentials(new_socket)

    result =
      case SessionService.authenticate(session_id, password) do
        {:ok, _session} ->
          {:noreply, new_socket, @timeout}

        {:error, _} = e ->
          Logger.debug("Invalid Handshake #{session_id}:#{password} (#{inspect(e)})")
          %Socket{transport: transport, transport_pid: transport_pid} = new_socket
          transport.shutdown(transport_pid, :read_write)
          {:stop, :normal, new_socket}
      end

    %Socket{transport_pid: transport_pid, transport: transport} = new_socket
    transport.setopts(transport_pid, active: :once)

    result
  end

  @impl true
  def handle_info({:tcp, transport_pid, message}, socket) do
    %Socket{id: id, transport_pid: ^transport_pid, transport: transport} = socket

    Logger.debug("New message from #{id} (len: #{byte_size(message)})")

    {:ok, decoded} = Socket.handle_in(message, socket)

    IO.inspect(socket)
    IO.inspect(decoded)

    # TODO: packet handling

    transport.setopts(transport_pid, active: :once)
    {:noreply, socket, @timeout}
  end

  def handle_info({:tcp_closed, transport_pid}, socket) do
    %Socket{id: id, transport_pid: ^transport_pid} = socket
    Logger.info("#{id} is now disconnected")
    {:stop, :normal, socket}
  end

  def handle_info(:timeout, socket) do
    %Socket{id: id, transport_pid: transport_pid, transport: transport} = socket
    Logger.error("An error occured with client #{id}: :timeout")
    transport.shutdown(transport_pid, :read_write)
    {:stop, {:shutdown, :timeout}, socket}
  end

  ## Private functions

  defp recv_session_key(socket) do
    {:ok, session_key} = Socket.recv(socket, 0, @handshake_timeout)
    if session_key != "0", do: Logger.warn("Session key is not 0", socket_id: socket.id)
    Socket.assign(socket, session_key: String.to_integer(session_key))
  end

  defp recv_credentials(new_socket) do
    {:ok, [session_id, password]} = Socket.recv(new_socket, 0, @handshake_timeout)
    hashed_password = :sha512 |> :crypto.hash(password) |> Base.encode16()
    {String.to_integer(session_id), hashed_password}
  end
end
