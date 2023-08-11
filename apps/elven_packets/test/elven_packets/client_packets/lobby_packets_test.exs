defmodule ElvenPackets.Client.LobbyPacketsTest do
  use ExUnit.Case, async: true

  alias ElvenGard.Network.Socket
  alias ElvenPackets.Client.LobbyPackets.{CharNEW, CharDEL, Select, GameStart}

  describe "Char_NEW" do
    test "can be deserialized" do
      params = "TestChar 0 1 1 2"
      assert %CharNEW{} = packet = CharNEW.deserialize("Char_NEW", params, %Socket{})
      assert packet.name == "TestChar"
      assert packet.slot == 0
      assert packet.gender == :female
      assert packet.hair_style == :hair_style_b
      assert packet.hair_color == :blue
    end
  end

  describe "Char_DEL" do
    test "can be deserialized" do
      params = "3 password"
      assert %CharDEL{} = packet = CharDEL.deserialize("Char_DEL", params, %Socket{})
      assert packet.slot == 3
      assert packet.password == "password"
    end
  end

  describe "select" do
    test "can be deserialized" do
      params = "2"
      assert %Select{} = packet = Select.deserialize("select", params, %Socket{})
      assert packet.slot == 2
    end
  end

  describe "game_start" do
    test "can be deserialized" do
      params = ""
      assert %GameStart{} = GameStart.deserialize("game_start", params, %Socket{})
    end
  end
end
