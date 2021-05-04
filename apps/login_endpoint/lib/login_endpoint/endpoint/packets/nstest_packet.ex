defmodule LoginEndpoint.Endpoint.NsTeSTPacket do
  @moduledoc """
  TODO: Documentation.
  """

  use Core.SerializablePacket

  alias __MODULE__
  alias LoginEndpoint.Endpoint.NsTeST.Channel

  @enforce_keys [:session_id, :username, :server_list]
  defstruct @enforce_keys

  @type t :: %NsTeSTPacket{
          session_id: non_neg_integer,
          username: String.t(),
          server_list: [Channel.t()]
        }

  @impl true
  def serialize(%NsTeSTPacket{} = struct, _) do
    %NsTeSTPacket{
      session_id: session_id,
      username: username,
      server_list: server_list
    } = struct

    serialized_servers = serialize_term(server_list, joiner: " ")
    "NsTeST #{session_id} #{username} #{serialized_servers} -1:-1:-1:10000.10000.1"
  end
end
