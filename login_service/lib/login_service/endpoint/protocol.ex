defmodule LoginService.Endpoint.Protocol do
  @moduledoc false

  use GenServer

  require Logger

  alias ElvenCore.Socket

  @behaviour :ranch_protocol

  @startup_timeout 5_000
  @timeout 5_000
  @separator [" ", "\v"]

  @packet_encoder LoginService.Endpoint.Cryptography
  @packet_schemas Application.fetch_env!(:login_service, :packet_schemas)

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

    Logger.metadata(socket_id: socket.id)
    Logger.info("New connection: #{:inet.ntoa(address)}:#{port}")

    transport.setopts(transport_pid, active: :once)
    :gen_server.enter_loop(__MODULE__, [], socket, @startup_timeout)
  end

  @impl true
  def handle_info({:tcp, transport_pid, message}, socket) do
    %Socket{transport_pid: ^transport_pid, transport: transport} = socket

    Logger.debug("New message (len: #{byte_size(message)})")

    with {:ok, {header, args}} <- parse_message(message, socket) do
      @packet_schemas.resolve(header, args, socket)
    else
      {:error, msg} -> Logger.warn(msg)
    end

    transport.setopts(transport_pid, active: :once)
    transport.shutdown(transport_pid, :read_write)

    {:noreply, socket, @timeout}
  end

  def handle_info({:tcp_closed, transport_pid}, socket) do
    %Socket{transport_pid: ^transport_pid} = socket
    Logger.info("Client disconnected")
    {:stop, :normal, socket}
  end

  def handle_info(:timeout, socket) do
    %Socket{transport_pid: transport_pid, transport: transport} = socket
    Logger.error("An error occured with the client: :timeout")
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
         split <- split_header(packet) do
      @packet_schemas.parse(split, socket, separator: @separator)
    end
  end

  defp split_header(packet) do
    case String.split(packet, @separator, parts: 2) do
      [header, bin_args] -> {header, bin_args}
      [header] -> {header, ""}
    end
  end

  defp decrypt_message(message, socket) do
    case Socket.handle_in(message, socket) do
      {:ok, "NoS0575 " <> _} = decrypted -> decrypted
      {:ok, "NoS0577 " <> _} = decrypted -> decrypted
      {:ok, packet} -> {:error, "Invalid packet received: #{inspect(packet)}"}
      e -> {:error, "Unable to decrypt login packet (#{inspect(e)})"}
    end
  end
end
