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
    session_key = recv_session_key(socket)
    new_socket = Socket.assign(socket, session_key: session_key)
    username = recv_username(new_socket)

    %Socket{transport_pid: transport_pid, transport: transport} = new_socket

    with {:ok, session} <- SessionService.authenticate(session_key, username),
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

    new_socket =
      with {:ok, packets} <- parse_message(message, socket) do
        Enum.reduce_while(packets, socket, &resolve_packet/2)
      else
        {:error, msg} ->
          Logger.warn(msg, socket_id: socket.id)
          socket
      end

    transport.setopts(transport_pid, active: :once)
    {:noreply, new_socket, @timeout}
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
    String.to_integer(session_key)
  end

  if Mix.env() == :dev do
    defp recv_username(new_socket) do
      {:ok, [username, _]} = Socket.recv(new_socket, 0, @handshake_timeout)
      [username, _] = String.split(username, " ", parts: 2)
      username
    end
  else
    defp recv_username(new_socket) do
      {:ok, [username, "thisisgfmode"]} = Socket.recv(new_socket, 0, @handshake_timeout)
      [username, "GF 0"] = String.split(username, " ", parts: 2)
      username
    end
  end

  defp parse_message(message, socket) do
    case Socket.handle_in(message, socket) do
      {:ok, packets} ->
        result =
          packets
          |> Stream.map(&String.replace_trailing(&1, "\n", ""))
          |> Stream.map(&String.replace_trailing(&1, " ", ""))
          |> Stream.map(&split_header/1)
          |> Enum.map(&@packet_schemas.parse(&1, socket, separator: @separator))

        {:ok, result}

      e ->
        {:error, "Unable to decrypt packets (#{inspect(e)})"}
    end
  end

  defp split_header("#" <> packet), do: packet |> String.replace("^", " ") |> split_header()
  defp split_header(packet) do
    case String.split(packet, @separator, parts: 2) do
      [header, bin_args] -> {header, bin_args}
      [header] -> {header, ""}
    end
  end

  defp resolve_packet({:ok, {header, args}}, socket) do
    case @packet_schemas.resolve(header, args, socket) do
      {:cont, %Socket{}} = x ->
        x

      {:halt, {:ok, _args}, %Socket{}} ->
        raise "unimplemented halt return"

      {:halt, {:error, _reason}, %Socket{}} ->
        raise "unimplemented halt return"

      x ->
        raise """
        handler for #{header} #{inspect(args)} must return `{:cont, socket}`, \
        `{:halt, {:ok, :some_args}, socket}`, or `{:halt, {:error, reason}, socket} `. \
        Returned: #{inspect(x)}
        """
    end
  end

  defp resolve_packet({:ignore, {header, args}}, socket) do
    split = String.split(args, @separator)
    Logger.debug("Ignored packet '#{header}' with args #{inspect(split)}")
    {:cont, socket}
  end

  defp resolve_packet({:error, :invalid, {header, args}}, socket) do
    split = String.split(args, @separator)
    Logger.warn("Unknown packet '#{header}' with args #{inspect(split)}", socket_id: socket.id)
    {:cont, socket}
  end
end
