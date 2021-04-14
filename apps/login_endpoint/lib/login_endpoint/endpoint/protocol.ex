defmodule LoginEndpoint.Endpoint.Protocol do
  @moduledoc false

  use GenServer

  require Logger

  alias Core.Socket
  alias LoginEndpoint.Endpoint.{Cryptography, PacketHandler}

  @behaviour :ranch_protocol

  @startup_timeout 5_000
  @timeout 5_000
  @separator [" ", "\v"]

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

    socket = Socket.new(transport, transport_pid)

    Logger.info("New connection: #{socket.id} (#{:inet.ntoa(address)}:#{port})")

    transport.setopts(transport_pid, active: :once)
    :gen_server.enter_loop(__MODULE__, [], socket, @startup_timeout)
  end

  @impl true
  def handle_info({:tcp, transport_pid, message}, socket) do
    %Socket{id: id, transport_pid: ^transport_pid, transport: transport} = socket

    Logger.debug("New message from #{id} (len: #{byte_size(message)})")

    with {:ok, {header, args}} <- parse_message(message, socket) do
      PacketHandler.handle_packet(header, args, socket)
    else
      {:error, msg} -> Logger.warn(msg)
    end

    transport.setopts(transport_pid, active: :once)
    transport.shutdown(transport_pid, :read_write)
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

  @spec parse_message(String.t(), map) ::
          {:ok, {header :: String.t(), args :: map}}
          | {:error, error :: atom}
  defp parse_message(message, socket) do
    with {:ok, decrypted} <- decrypt_message(message, socket),
         packet <- String.replace_trailing(decrypted, "\n", ""),
         splitted <- String.split(packet, @separator) do
      prepare_args(splitted, socket)
    end
  end

  defp decrypt_message(message, socket) do
    case Cryptography.decrypt(message) do
      "NoS0575 " <> _ = decrypted -> {:ok, decrypted}
      _ -> {:error, "Unable to decrypt login packet from #{socket.id}"}
    end
  end

  defp prepare_args(splitted, socket) when length(splitted) != 9 do
    {:error, "Invalid packet args length for #{socket.id}"}
  end

  defp prepare_args(["NoS0575" = header | str_args], _socket) do
    [
      session_id,
      username,
      password,
      guid,
      _,
      client_version,
      "0",
      client_checksum
    ] = str_args

    args = %{
      session_id: String.to_integer(session_id),
      username: username,
      password: password,
      guid: guid,
      client_version: client_version,
      client_checksum: client_checksum
    }

    {:ok, {header, args}}
  end
end
