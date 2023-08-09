defmodule ElvenPackets.Client.LobbyPacketsTest do
  use ExUnit.Case, async: true

  alias ElvenGard.Network.Socket
  alias ElvenPackets.Client.LobbyPackets.{CharNEW, CharDEL, Select, GameStart}

  test "can deserialize Char_NEW" do
    params = "TestChar 0 1 1 2"
    assert %CharNEW{} = packet = Char_NEW.deserialize("Char_NEW", params, %Socket{})
    assert packet.name == "TestChar"
    assert packet.slot == 0
    assert packet.gender == 1
    assert packet.hair_style == 1
    assert packet.hair_color == 2
  end

  test "can deserialize Char_DEL" do
    params = "3 password"
    assert %CharDEL{} = packet = Char_DEL.deserialize("Char_DEL", params, %Socket{})
    assert packet.slot == 3
    assert packet.password == "password"
  end

  test "can deserialize select" do
    params = "2"
    assert %Select{} = packet = select.deserialize("select", params, %Socket{})
    assert packet.slot == 2
  end

  test "can deserialize game_start" do
    assert %GameStart{} = game_start.deserialize("game_start", nil, %Socket{})
  end
end
