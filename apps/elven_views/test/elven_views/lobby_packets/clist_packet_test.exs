defmodule ElvenViews.LobbyPackets.ClistPacketTest do
  use PacketCase, async: true

  alias ElvenViews.SubPackets.EquipmentSubPacket
  alias ElvenViews.LobbyPackets.ClistPacket

  ## Tests

  describe "serialize/2" do
    test "can serialize a packet structure" do
      packet = structure_to_iolist(clist_mock())

      assert is_list(packet)
      assert length(packet) == 17
      assert packet_index(packet, 0) == "clist"

      # field :slot
      assert packet_index(packet, 1) == "1"
      # field :name
      assert packet_index(packet, 2) == "admin"
      # field :unknown1
      assert packet_index(packet, 3) == "0"
      # field :gender
      assert packet_index(packet, 4) == "1"
      # field :hair_style
      assert packet_index(packet, 5) == "3"
      # field :hair_color
      assert packet_index(packet, 6) == "9"
      # field :unknown2
      assert packet_index(packet, 7) == "0"
      # field :class
      assert packet_index(packet, 8) == "4"
      # field :level
      assert packet_index(packet, 9) == "11"
      # field :hero_level
      assert packet_index(packet, 10) == "22"
      # field :equipments
      assert packet_index(packet, 11) == "339.4479.4983.4980.227.4131.4402.4401.-1.4443"
      # field :job_level
      assert packet_index(packet, 12) == "33"
      # field :quest_completion
      assert packet_index(packet, 13) == "1"
      # field :quest_part
      assert packet_index(packet, 14) == "1"
      # field :pets
      assert packet_index(packet, 15) == "-1"
      # field :design
      assert packet_index(packet, 16) == "44"
    end
  end

  ## Helpers

  defp clist_mock() do
    equipments = %EquipmentSubPacket{
      hat: 339,
      armor: 4479,
      main_weapon: 4983,
      secondary_weapon: 4980,
      mask: 227,
      fairy: 4131,
      costume_suit: 4402,
      costume_hat: 4401,
      weapon_skin: nil,
      wings_skin: 4443
    }

    %ClistPacket{
      slot: 1,
      name: "admin",
      gender: :female,
      hair_style: :hair_style_d,
      hair_color: :pink_red,
      class: :martial_artist,
      level: 11,
      hero_level: 22,
      equipments: equipments,
      job_level: 33,
      # TODO: test pets
      pets: [],
      design: 44
    }
  end
end
