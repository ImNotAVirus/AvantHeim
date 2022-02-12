defmodule LoginEndpoint.PacketSchemasTest do
  use ExUnit.Case, async: true

  alias LoginEndpoint.PacketSchemas

  @separator [" ", "\v"]

  ## Tests

  describe "packet NoS0575" do
    test "can be parsed" do
      sha512_hash = :crypto.hash(:sha512, "data") |> Base.encode16()
      md5_hash = :crypto.hash(:md5, "data") |> Base.encode16()
      guid = Core.UUID.uuid4()

      parsed =
        "NoS0575 4745632 admin #{sha512_hash} #{guid} 0047BA11 0\v0.9.3.3086 0 #{md5_hash}"
        |> split_header()
        |> parse()

      expected =
        {"NoS0575",
         %{
           session_id: 4_745_632,
           username: "admin",
           password: sha512_hash,
           installation_guid: guid,
           unknown: "0047BA11",
           region_code: 0,
           client_version: "0.9.3.3086",
           always_0: "0",
           client_checksum: md5_hash
         }}

      assert {:ok, ^expected} = parsed
    end
  end

  describe "packet NoS0577" do
    test "can be parsed" do
      md5_hash = :crypto.hash(:md5, "data") |> Base.encode16()
      guid = Core.UUID.uuid4()

      parsed =
        "NoS0577 6465616462656566  #{guid} 006C993A 2\v0.9.3.3147 0 #{md5_hash}"
        |> split_header()
        |> parse()

      expected =
        {"NoS0577",
         %{
           token: "6465616462656566",
           empty: "",
           installation_guid: guid,
           unknown: "006C993A",
           region_code: 2,
           client_version: "0.9.3.3147",
           always_0: "0",
           client_checksum: md5_hash
         }}

      assert {:ok, ^expected} = parsed
    end
  end

  ## Helpers

  defp split_header(packet) do
    case String.split(packet, @separator, parts: 2) do
      [header, bin_args] -> {header, bin_args}
      [header] -> {header, ""}
    end
  end

  defp parse(packet) do
    PacketSchemas.parse(packet, nil, separator: @separator)
  end
end
