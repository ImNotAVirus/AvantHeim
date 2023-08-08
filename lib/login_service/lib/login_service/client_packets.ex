defmodule LoginService.ClientPackets do
  @moduledoc """
  TODO: LoginService.ClientPacket
  """

  use ElvenGard.Network.PacketSerializer

  alias LoginService.Types.{NsInteger, NsString}

  #######
  # The login packet (GameForge old client)
  # ---
  # Example: "NoS0575 4745632 admin [sha512_hash] [guid] 0047BA11 0\v0.9.3.3086 0 [md5_hash]"
  #######
  if Mix.env() != :prod do
    @deserializable true
    packet "NoS0575" do
      field :session_id, NsInteger
      field :username, NsString
      field :password, NsString
      field :installation_guid, NsString
      field :unknown, NsString
      field :region_code, NsInteger
      field :client_version, NsString
      field :always_0, NsString, using: "0"
      field :client_checksum, NsString
    end
  end

  #######
  # The login packet (GameForge new clients)
  # ---
  # Example: "NoS0577 6465616462656566  61f80977-8f3d-4643-99ea-8b0b9227c49e 006C993A 2\v0.9.3.3147 0 749389B569B0668FD52FD037BB02FCB3"
  #######
  @deserializable true
  packet "NoS0577" do
    field :token, NsString
    field :empty, NsString, using: ""
    field :installation_guid, NsString
    field :unknown, NsString
    field :region_code, NsInteger
    field :client_version, NsString
    field :always_0, NsString, using: "0"
    field :client_checksum, NsString
  end
end
