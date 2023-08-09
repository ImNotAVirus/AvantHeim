defmodule ElvenPackets.Client.AreaPacketsTest do
  use ExUnit.Case, async: true

  alias ElvenGard.Network.Socket
  alias ElvenPackets.Client.AreaPackets.{Walk, Say, Ncif, Guri, Dir}

  test "can deserialize walk" do
    params = "3 1 313 25"
    assert %Walk{} = packet = Walk.deserialize("walk", params, %Socket{})
    assert packet.pos_x == 3
    assert packet.pos_y == 1
    assert packet.checksum == 313
    assert packet.speed == 25
  end

  test "can deserialize say" do
    params = "DarkyZ"
    assert %Say{} = packet = Say.deserialize("say", params, %Socket{})
    assert packet.message == "DarkyZ"
  end

  test "can deserialize ncif" do
    params = "1 123"
    assert %Ncif{} = packet = Ncif.deserialize("ncif", params, %Socket{})
    assert packet.entity_type == 1
    assert packet.entity_id == 123
  end

  test "can deserialize guri" do
    params = "10 2 1 0"
    assert %Guri{} = packet = Guri.deserialize("guri", params, %Socket{})
    assert packet.type == 10
    assert packet.entity_type == 2
    assert packet.entity_id == 1
    assert packet.guri_data == 0
  end

  test "can deserialize dir" do
    params = "1 2 3"
    assert %Dir{} = packet = Dir.deserialize("dir", params, %Socket{})
    assert packet.dir == 1
    assert packet.entity_type == 2
    assert packet.entity_id == 3
  end
end
