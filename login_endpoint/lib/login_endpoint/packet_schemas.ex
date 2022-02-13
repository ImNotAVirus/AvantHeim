defmodule LoginEndpoint.PacketSchemas do
  @moduledoc """
  TODO: Documentation
  """

  use ElvenCore.PacketSchema

  alias LoginEndpoint.Endpoint.AuthActions

  #######
  # The login packet (GameForge old client)
  # ---
  # Example: "NoS0575 4745632 admin [sha512_hash] [guid] 0047BA11 0\v0.9.3.3086 0 [md5_hash]"
  #######
  if Mix.env() != :prod do
    packet "NoS0575" do
      field :session_id, :integer
      field :username, :string
      field :password, :string
      field :installation_guid, :string
      field :unknown, :string
      field :region_code, :integer
      field :client_version, :string
      field :always_0, :string, using: "0"
      field :client_checksum, :string

      resolve AuthActions, :login
    end
  end

  #######
  # The login packet (GameForge new clients)
  # ---
  # Example: "NoS0577 6465616462656566  61f80977-8f3d-4643-99ea-8b0b9227c49e 006C993A 2\v0.9.3.3147 0 749389B569B0668FD52FD037BB02FCB3"
  #######
  packet "NoS0577" do
    field :token, :string
    field :empty, :string, using: ""
    field :installation_guid, :string
    field :unknown, :string
    field :region_code, :integer
    field :client_version, :string
    field :always_0, :string, using: "0"
    field :client_checksum, :string

    resolve AuthActions, :login
  end
end
