defmodule ElvenViews.LoginPackets.NsTeSTPacket do
  @moduledoc """
  TODO: Documentation.
  """

  use ElvenCore.SerializableStruct

  alias __MODULE__
  alias ElvenViews.LoginPackets.NsTeST.Channel

  # TODO: region & auth_type must be enums!!!!
  # region = 0 - International (cf. NoS0577)
  # 0 = ORG, 1 = STEAM, 2 = GF
  @enforce_keys [:encryption_key, :username, :server_list]
  @additional_keys [region: 0, auth_type: 2]
  defstruct @enforce_keys ++ @additional_keys

  @type t :: %NsTeSTPacket{
          encryption_key: non_neg_integer,
          username: String.t(),
          server_list: [Channel.t()]
        }

  @impl true
  def serialize(%NsTeSTPacket{} = struct, _) do
    %NsTeSTPacket{
      region: region,
      username: username,
      auth_type: auth_type,
      encryption_key: encryption_key,
      server_list: server_list
    } = struct

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
      [unused_servers, 0, encryption_key],
      serialize_term(server_list, joiner: " "),
      terminator
    ])
  end
end
