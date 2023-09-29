defmodule ElvenPackets.Client.UiPacketsTest do
  use ElvenPackets.PacketCase, async: true

  alias ElvenPackets.SubPackets.I18nSubPacket
  alias ElvenPackets.Server.UiPackets.{Cancel, Gb, Gold, Info, Scene, SMemoi, SMemoi2}

  # Tests

  describe "cancel" do
    test "can be serialized" do
      packet = %Cancel{cancel_type: :skill, entity_id: 50}
      assert {"cancel", params} = serialize_packet(packet)
      assert is_list(params)
      assert length(params) == 3
      assert Enum.at(params, 0) == "0"
      assert Enum.at(params, 1) == "50"
      assert Enum.at(params, 2) == "-1"
    end
  end

  describe "gb" do
    test "can be serialized" do
      packet = %Gb{
        action_type: :deposit_gold,
        bank_gold: 50000,
        gold: 500,
        bank_rank: 1,
        bank_tax: 10000
      }

      assert {"gb", params} = serialize_packet(packet)
      assert is_list(params)
      assert length(params) == 5
      assert Enum.at(params, 0) == "1"
      assert Enum.at(params, 1) == "50000"
      assert Enum.at(params, 2) == "500"
      assert Enum.at(params, 3) == "1"
      assert Enum.at(params, 4) == "10000"
    end
  end

  describe "gold" do
    test "can be serialized" do
      packet = %Gold{gold: 50, bank_gold: 500}
      assert {"gold", params} = serialize_packet(packet)
      assert is_list(params)
      assert length(params) == 2
      assert Enum.at(params, 0) == "50"
      assert Enum.at(params, 1) == "500"
    end
  end

  describe "info" do
    test "can be serialized" do
      packet = %Info{message: "Some message"}
      assert {"info", params} = serialize_packet(packet)
      assert is_list(params)
      assert length(params) == 1
      assert Enum.at(params, 0) == "Some message"
    end
  end

  describe "s_memoi" do
    test "can be serialized" do
      packet = %SMemoi{text_color: :red, i18n_packet: %I18nSubPacket{key: "SkillDisapeared"}}
      assert {"s_memoi", params} = serialize_packet(packet)
      assert is_list(params)
      assert length(params) == 2
      assert Enum.at(params, 0) == "5"
      assert Enum.at(params, 1) == ["18", "0"]
    end
  end

  describe "s_memoi2" do
    test "can be serialized" do
      packet = %SMemoi2{
        text_color: :white,
        i18n_packet: %I18nSubPacket{key: "NotEnoughKoarenTreasure"}
      }

      assert {"s_memoi2", params} = serialize_packet(packet)
      assert is_list(params)
      assert length(params) == 2
      assert Enum.at(params, 0) == "6"
      assert Enum.at(params, 1) == ["20", "0"]
    end
  end

  describe "scene" do
    test "can be serialized" do
      packet = %Scene{scene_id: 1, cancellable: true}
      assert {"scene", params} = serialize_packet(packet)
      assert is_list(params)
      assert length(params) == 2
      assert Enum.at(params, 0) == "1"
      assert Enum.at(params, 1) == "1"
    end
  end
end
