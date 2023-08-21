defmodule ElvenPackets.Server.LobbyPacketsTest do
  use ElvenPackets.PacketCase, async: true

  alias ElvenPackets.Server.LobbyPackets.{CList, CListEnd, CListStart, Ok}
  alias ElvenPackets.SubPackets.Equipment

  ## Tests

  describe "OK" do
    test "can be serialized" do
      assert {"OK", []} = serialize_packet(%Ok{})
    end
  end

  describe "clist_start" do
    test "can be serialized" do
      assert {"clist_start", ["0"]} = serialize_packet(%CListStart{})
    end
  end

  describe "clist_end" do
    test "can be serialized" do
      assert {"clist_end", []} = serialize_packet(%CListEnd{})
    end
  end

  describe "clist" do
    test "can be serialized" do
      equipment = %Equipment{
        hat: 1,
        armor: 2,
        main_weapon: 3,
        secondary_weapon: 4,
        mask: 5,
        fairy: 6,
        costume_suit: 7,
        costume_hat: 8,
        weapon_skin: 9,
        wings_skin: 10
      }

      packet = %CList{
        slot: 0,
        name: "DarkyZ",
        gender: :female,
        hair_style: :hair_style_d,
        hair_color: :light_blue,
        class: :archer,
        level: 12,
        hero_level: 23,
        equipments: equipment,
        job_level: 34,
        pets: [],
        design: 45
      }

      assert {"clist", params} = serialize_packet(packet)
      assert Enum.at(params, 0) == "0"
      assert Enum.at(params, 1) == "DarkyZ"
      assert Enum.at(params, 2) == "0"
      assert Enum.at(params, 3) == "1"
      assert Enum.at(params, 4) == "3"
      assert Enum.at(params, 5) == "8"
      assert Enum.at(params, 6) == "0"
      assert Enum.at(params, 7) == "2"
      assert Enum.at(params, 8) == "12"
      assert Enum.at(params, 9) == "23"
      assert Enum.at(params, 10) == "1.2.3.4.5.6.7.8.9.10"
      assert Enum.at(params, 11) == "34"
      assert Enum.at(params, 12) == "1"
      assert Enum.at(params, 13) == "1"
      assert Enum.at(params, 14) == "-1"
      assert Enum.at(params, 15) == "45"
    end
  end
end
