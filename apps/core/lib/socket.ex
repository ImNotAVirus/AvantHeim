defmodule Core.Socket do
  @moduledoc ~S"""
  Manage a socket

  ## Socket fields

    * `:id` - The string id of the socket
    * `:transport` - A [Ranch transport](https://ninenines.eu/docs/en/ranch/2.0/guide/transports/)
    * `:transport_pid` - The pid of the socket's transport process
    * `:assigns` - The map of socket assigns, default: `%{}`
  """

  alias Core.{Socket, UUID}

  @enforce_keys [:id, :transport, :transport_pid]

  defstruct id: nil,
            transport: nil,
            transport_pid: nil,
            assigns: %{}

  @type t :: %Socket{
          id: String.t(),
          transport: atom,
          transport_pid: port,
          assigns: map
        }

  @doc """
  Create a new structure
  """
  @spec new(atom, port) :: Socket.t()
  def new(transport, transport_pid) do
    %Socket{
      id: UUID.uuid4(),
      transport: transport,
      transport_pid: transport_pid
    }
  end

  @doc """
  Send a packet to the client.

  ## Examples

      iex> send(socket, "data")
  """
  @spec send(Socket.t(), any) :: :ok | {:error, atom}
  def send(%Socket{} = socket, message) do
    %Socket{transport: transport, transport_pid: transport_pid} = socket
    transport.send(transport_pid, message)
  end

  @doc """
  Adds key value pairs to socket assigns.

  A single key value pair may be passed, a keyword list or map
  of assigns may be provided to be merged into existing socket
  assigns.

  ## Examples

      iex> assign(socket, :name, "ElvenGard")
      iex> assign(socket, name: "ElvenGard", logo: "🌸")
  """
  @spec assign(Socket.t(), atom, any) :: Socket.t()
  def assign(%Socket{} = socket, key, value) when is_atom(key) do
    assign(socket, [{key, value}])
  end

  @spec assign(Socket.t(), map | keyword) :: Socket.t()
  def assign(%Socket{} = socket, attrs) when is_map(attrs) or is_list(attrs) do
    %{socket | assigns: Map.merge(socket.assigns, Map.new(attrs))}
  end
end
