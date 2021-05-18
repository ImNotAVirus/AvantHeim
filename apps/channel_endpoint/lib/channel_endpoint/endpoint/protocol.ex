defmodule ChannelEndpoint.Endpoint.Protocol do
  @moduledoc false

  use GenServer

  require Logger

  alias Core.Socket
  alias SessionService.Session
  alias ChannelEndpoint.Endpoint.LobbyViews
  alias DatabaseService.Players.{Account, Accounts, Characters}

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

    %Socket{transport_pid: transport_pid, transport: transport} = new_socket

    with {:ok, session} <- SessionService.authenticate(session_id, password),
         {:ok, account} <- get_account(session),
         :ok <- send_character_list(account, new_socket) do
      transport.setopts(transport_pid, active: :once)
      {:noreply, Socket.assign(new_socket, account: account), @timeout}
    else
      e ->
        Logger.info("Invalid Handshake (reason: #{inspect(e)})", socket_id: socket.id)
        transport.shutdown(transport_pid, :read_write)
        {:stop, :normal, new_socket}
    end
  end

  @impl true
  def handle_info({:tcp, transport_pid, message}, socket) do
    %Socket{id: id, transport_pid: ^transport_pid, transport: transport} = socket

    Logger.debug("New message from #{id} (len: #{byte_size(message)})")

    with {:ok, packets} <- parse_message(message, socket) do
      Enum.each(packets, fn
        {:ok, {header, args}} ->
          @packet_schemas.resolve(header, args, socket)

        {:error, :invalid, [header | args]} ->
          Logger.warn("Invalid packet '#{header}' with args #{inspect(args)}",
            socket_id: socket.id
          )
      end)
    else
      {:error, msg} -> Logger.warn(msg, socket_id: socket.id)
    end

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

  defp get_account(%Session{} = session) do
    %Session{username: username, password: password} = session

    case Accounts.log_in(username, password) do
      %Account{} = acc -> {:ok, acc}
      _ -> {:error, :cant_fetch_account}
    end
  end

  defp send_character_list(%Account{} = account, socket) do
    character_list = Characters.all_by_account_id(account.id)
    Socket.send(socket, LobbyViews.render(:clist_start, nil))
    Enum.each(character_list, &Socket.send(socket, LobbyViews.render(:clist, &1)))
    Socket.send(socket, LobbyViews.render(:clist_end, nil))
  end

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

  defp parse_message(message, socket) do
    case Socket.handle_in(message, socket) do
      {:ok, packets} ->
        result =
          packets
          |> Stream.map(&String.replace_trailing(&1, "\n", ""))
          |> Stream.map(&String.replace_trailing(&1, " ", ""))
          |> Stream.map(&String.split(&1, @separator))
          |> Enum.map(&@packet_schemas.parse_packet_args(&1, socket))

        {:ok, result}

      e ->
        {:error, "Unable to decrypt login packet (#{inspect(e)})"}
    end
  end
end
