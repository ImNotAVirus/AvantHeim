defmodule ElvenPackets.Client.CharacterCdPacketsTest do
  use ExUnit.Case, async: true

  alias ElvenGard.Network.Socket
  alias ElvenPackets.Client.CharacterCdPackets.{Char_NEW, Char_DEL}

  test "can deserialize Char_NEW" do
    params = "TestChar 0 1 1 2"
    assert %Char_NEW{} = packet = Char_NEW.deserialize("Char_NEW", params, %Socket{})
    assert packet.name == "TestChar"
    assert packet.slot == 0
    assert packet.gender == 1
    assert packet.hair_style == 1
    assert packet.hair_color == 2
  end

  test "can deserialize Char_DEL" do
    params = "3 password"
    assert %Char_DEL{} = packet = Char_DEL.deserialize("Char_DEL", params, %Socket{})
    assert packet.slot == 3
    assert packet.password == "password"
  end
end
