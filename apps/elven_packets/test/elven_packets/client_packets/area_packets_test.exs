defmodule ElvenPackets.Client.AreaPacketsTest do
  use ExUnit.Case, async: true

  alias ElvenGard.Network.Socket
  alias ElvenPackets.Client.AreaPackets.{Walk, Say, Ncif, Guri, Dir, Preq}

  describe "preq" do
    test "can be deserialized" do
      assert %Preq{} = Preq.deserialize("preq", "", %Socket{})
    end
  end

  describe "walk" do
    test "can be deserialized" do
      params = "3 1 313 25"
      assert %Walk{} = packet = Walk.deserialize("walk", params, %Socket{})
      assert packet.pos_x == 3
      assert packet.pos_y == 1
      assert packet.checksum == 313
      assert packet.speed == 25
    end
  end

  describe "say" do
    test "can be deserialized" do
      params = "DarkyZ"
      assert %Say{} = packet = Say.deserialize("say", params, %Socket{})
      assert packet.message == "DarkyZ"
    end

    test "can be deserialized with spaces" do
      params = "This is a test"
      assert %Say{} = packet = Say.deserialize("say", params, %Socket{})
      assert packet.message == "This is a test"
    end
  end

  describe "ncif" do
    test "can be deserialized" do
      params = "1 123"
      assert %Ncif{} = packet = Ncif.deserialize("ncif", params, %Socket{})
      assert packet.entity_type == :player
      assert packet.entity_id == 123
    end
  end

  describe "guri" do
    test "can be deserialized" do
      params = "10 2 1 0"
      assert %Guri{} = packet = Guri.deserialize("guri", params, %Socket{})
      assert packet.type == :emoji
      assert packet.entity_type == :npc
      assert packet.entity_id == 1
      assert packet.guri_data == 0
    end
  end

  describe "dir" do
    test "can be deserialized" do
      params = "1 2 3"
      assert %Dir{} = packet = Dir.deserialize("dir", params, %Socket{})
      assert packet.direction == :east
      assert packet.entity_type == :npc
      assert packet.entity_id == 3
    end
  end
end
