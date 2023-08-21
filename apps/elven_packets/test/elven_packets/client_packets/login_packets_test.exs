defmodule ElvenPackets.Client.LoginPacketsTest do
  use ExUnit.Case, async: true

  alias ElvenGard.Network.Socket
  alias ElvenPackets.Client.LoginPackets.{NoS0575, NoS0577}

  test "can deserialize NoS0575" do
    params = "4745632 admin [sha512_hash] [guid] 0047BA11 0\v0.9.3.3086 0 [md5_hash]"
    assert %NoS0575{} = packet = NoS0575.deserialize("NoS0575", params, %Socket{})
    assert packet.session_id == 4_745_632
    assert packet.username == "admin"
    assert packet.password == "[sha512_hash]"
    assert packet.installation_guid == "[guid]"
    assert packet.unknown == "0047BA11"
    assert packet.region == :en
    assert packet.client_version == "0.9.3.3086"
    assert packet.always_0 == "0"
    assert packet.client_checksum == "[md5_hash]"
  end

  test "can deserialize NoS0577" do
    params =
      "6465616462656566  61f80977-8f3d-4643-99ea-8b0b9227c49e 006C993A 2\v0.9.3.3147 0 749389B569B0668FD52FD037BB02FCB3"

    assert %NoS0577{} = packet = NoS0577.deserialize("NoS0577", params, %Socket{})
    assert packet.token == "6465616462656566"
    assert packet.empty == ""
    assert packet.installation_guid == "61f80977-8f3d-4643-99ea-8b0b9227c49e"
    assert packet.unknown == "006C993A"
    assert packet.region == :fr
    assert packet.client_version == "0.9.3.3147"
    assert packet.always_0 == "0"
    assert packet.client_checksum == "749389B569B0668FD52FD037BB02FCB3"
  end
end
