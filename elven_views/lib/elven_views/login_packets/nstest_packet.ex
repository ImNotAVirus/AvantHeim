defmodule ElvenViews.LoginPackets.NsTeSTPacket do
  @moduledoc """
  TODO: Documentation.
  """

  use ElvenViews.SerializablePacket

  alias ElvenViews.LoginPackets.NsTeST.Channel

  ## Packet definition

  # TODO: region & auth_type must be enums!!!!
  defpacket "NsTeST" do
    # region: 0 - International (cf. NoS0577)
    field :region, :non_neg_integer, default: 0
    field :username, :string
    # auth_type: 0 = ORG, 1 = STEAM, 2 = GF
    field :auth_type, :non_neg_integer, default: 2
    field :server1, :string, default: empty_server()
    field :server2, :string, default: empty_server()
    field :server3, :string, default: empty_server()
    field :server4, :string, default: empty_server()
    field :server5, :string, default: empty_server()
    field :server6, :string, default: empty_server()
    field :unused_servers, :string, default: unused_servers()
    field :unknown, :integer, default: 0
    field :encryption_key, :non_neg_integer
    field :server_list, :list, type: Channel, joiner: " "
    field :terminator, :string, default: "-1:-1:-1:10000.10000.1"
  end

  ## Private functions

  def empty_server() do
    "-99 0 -99 0 -99 0 -99 0"
  end

  def unused_servers() do
    [empty_server()]
    |> Stream.cycle()
    |> Enum.take(3)
    |> Enum.join(" ")
  end
end
