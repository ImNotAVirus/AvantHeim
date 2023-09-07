defmodule ElvenPackets.Client.LoginPackets do
  @moduledoc """
  TODO: ElvenPackets.Client.LoginPackets
  """

  use ElvenGard.Network.PacketSerializer

  import ElvenPackets.Enums.LoginEnums, only: [login_region: 1]

  alias ElvenPackets.Types.{NsEnum, NsInteger, NsString}

  #######
  # The login packet (GameForge old client)
  # ---
  # Example: "NoS0575 4745632 admin [sha512_hash] [guid] 0047BA11 0\v0.9.3.3086 0 [md5_hash]"
  #######
  # FIXME: THIS FUNCTION MUST BE DISABLED IN PROD
  # if Mix.env() != :prod do
  @deserializable true
  defpacket "NoS0575", as: NoS0575 do
    field(:session_id, NsInteger)
    field(:username, NsString)
    field(:password, NsString)
    field(:installation_guid, NsString)
    field(:unknown, NsString)
    field(:region, NsEnum, values: login_region(:__enumerators__))
    field(:client_version, NsString)
    # , using: "0"
    field(:always_0, NsString)
    field(:client_checksum, NsString)
  end

  # end

  #######
  # The login defpacket (GameForge new clients)
  # ---
  # Example: "NoS0577 6465616462656566  61f80977-8f3d-4643-99ea-8b0b9227c49e 006C993A 2\v0.9.3.3147 0 749389B569B0668FD52FD037BB02FCB3"
  #######
  @deserializable true
  defpacket "NoS0577", as: NoS0577 do
    field(:token, NsString)
    # , using: ""
    field(:empty, NsString)
    field(:installation_guid, NsString)
    field(:unknown, NsString)
    field(:region, NsEnum, values: login_region(:__enumerators__))
    field(:client_version, NsString)
    # , using: "0"
    field(:always_0, NsString)
    field(:client_checksum, NsString)
  end
end
