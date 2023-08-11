defmodule ElvenPackets.Server.ChatPacketsTest do
  use ElvenPackets.PacketCase, async: true
  
  alias ElvenPackets.Server.ChatPackets.{Bn, Say}

  ## Tests
  
  describe "bn" do
    test "can be serialized" do
      packet = %Bn{id: 123, message: "Hello^welcome^to^AvantHeim"}
      assert {"bn", params} = serialize_packet(packet)
      assert is_list(params)
      assert length(params) == 2
      assert Enum.at(params, 0) == "123"
      assert Enum.at(params, 1) == "Hello^welcome^to^AvantHeim"
    end
  end
  
  describe "say" do
    test "can be serialized" do
      packet = %Say{entity_type: :character, entity_id: 123, color: :special_gold, message: "This is a message for the SayPacket"}
      assert {"say", params} = serialize_packet(packet)
      assert is_list(params)
      assert length(params) == 4
      assert Enum.at(params, 0) == "1"
      assert Enum.at(params, 1) == "123"
      assert Enum.at(params, 2) == "10"
      assert Enum.at(params, 3) == "This is a message for the SayPacket"
    end
  end
  
  ## Helpers
  
end
