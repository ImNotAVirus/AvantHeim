defmodule LoginEndpoint.PacketSchemas do
  @moduledoc """
  TODO: Documentation
  """

  use Core.PacketSchema

  alias LoginEndpoint.Endpoint.PacketHandler

  @doc """
  The login packet (GameForge old client)

  Example: "NoS0575 4745632 admin [sha512_hash] 0047BA11\v0.9.3.3086 0 [md5_hash]"
  """
  packet "NoS0575P" do
    field :session_id, :integer
    field :username, :string
    field :password, :string
    field :guid, :string
    field :unknown, :string
    field :client_version, :string
    field :always_0, :string, using: "0"
    field :client_checksum, :string

    resolve PacketHandler, :handle_packet
  end
end
