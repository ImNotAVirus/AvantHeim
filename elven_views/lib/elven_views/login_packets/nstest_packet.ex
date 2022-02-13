defmodule ElvenViews.LoginPackets.NsTeSTPacket do
  @moduledoc """
  TODO: Documentation.
  """

  use ElvenCore.SerializableStruct

  alias __MODULE__
  alias ElvenViews.LoginPackets.NsTeST.Channel

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

    # International (cf. NoS0577)
    region = 0
    # 0 = ORG, 1 = STEAM, 2 = GF
    auth_type = 2

    serialized_servers = serialize_term(server_list, joiner: " ")
    terminator = "-1:-1:-1:10000.10000.1"

    unused_servers =
      ["-99 0 -99 0 -99 0 -99 0"]
      |> Stream.cycle()
      |> Enum.take(3)
      |> Enum.join(" ")

    # TODO: Implement characters count
    # "<channel_id> <characters_count> x4"
    List.flatten([
      ["NsTeST", region, username, auth_type],
      ["-99 0 -99 0 -99 0 -99 0"],
      ["-99 0 -99 0 -99 0 -99 0"],
      ["-99 0 -99 0 -99 0 -99 0"],
      ["-99 0 -99 0 -99 0 -99 0"],
      ["-99 0 -99 0 -99 0 -99 0"],
      ["-99 0 -99 0 -99 0 -99 0"],
      [unused_servers],
      [0, session_id, serialized_servers],
      terminator
    ])
  end
end
