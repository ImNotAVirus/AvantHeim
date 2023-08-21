defmodule ElvenViews.VisibilityPackets.InCharacterPacketTest do
  use PacketCase, async: true

  alias ElvenViews.SubPackets.EquipmentSubPacket
  alias ElvenViews.SubPackets.FamilyIdRankSubPacket
  alias ElvenViews.SubPackets.ItemUpgradeRaritySubPacket

  alias ElvenViews.VisibilityPackets.InCharacterPacket

  ## Tests

  describe "serialize/2" do
    test "can serialize a packet structure" do
      packet = structure_to_iolist(in_mock())

      assert is_list(packet)
      assert length(packet) == 41
      assert packet_index(packet, 0) == "in"

      # field :entity_type
      assert packet_index(packet, 1) == "1"
      # field :name
      assert packet_index(packet, 2) == "admin"
      # field :vnum
      assert packet_index(packet, 3) == "-"
      # field :entity_id
      assert packet_index(packet, 4) == "203210"
      # field :map_x
      assert packet_index(packet, 5) == "19"
      # field :map_y
      assert packet_index(packet, 6) == "23"
      # field :direction
      assert packet_index(packet, 7) == "2"
      # field :authority
      assert packet_index(packet, 8) == "0"
      # field :gender
      assert packet_index(packet, 9) == "1"
      # field :hair_style
      assert packet_index(packet, 10) == "3"
      # field :hair_color
      assert packet_index(packet, 11) == "9"
      # field :class
      assert packet_index(packet, 12) == "4"
      # field :equipments
      assert packet_index(packet, 13) == "339.4479.4983.4980.227.4131.4402.4401.-1.4443"
      # field :hp_percent
      assert packet_index(packet, 14) == "50"
      # field :mp_percent
      assert packet_index(packet, 15) == "60"
      # field :is_sitting
      assert packet_index(packet, 16) == "0"
      # field :group_id
      assert packet_index(packet, 17) == "9999"
      # field :fairy_move_type_id
      assert packet_index(packet, 18) == "1"
      # field :fairy_element
      assert packet_index(packet, 19) == "3"
      # field :unknown1
      assert packet_index(packet, 20) == "0"
      # field :fairy_morph
      assert packet_index(packet, 21) == "42"
      # field :spawn_effect
      assert packet_index(packet, 22) == "2"
      # field :morph
      assert packet_index(packet, 23) == "13"
      # field :weapon_upgrade
      assert packet_index(packet, 24) == "56"
      # field :armor_upgrade
      assert packet_index(packet, 25) == "78"
      # field :family_id_rank
      assert packet_index(packet, 26) == "124.915"
      # field :family_name
      assert packet_index(packet, 27) == "ElvenGard"
      # field :reputation_icon_id
      assert packet_index(packet, 28) == "20"
      # field :is_invisible
      assert packet_index(packet, 29) == "0"
      # field :morph_upgrade
      assert packet_index(packet, 30) == "15"
      # field :faction
      assert packet_index(packet, 31) == "0"
      # field :morph_design
      assert packet_index(packet, 32) == "8"
      # field :level
      assert packet_index(packet, 33) == "99"
      # field :family_level
      assert packet_index(packet, 34) == "20"
      # field :family_icons
      assert packet_index(packet, 35) == "1|0|1"
      # field :is_arena_winner
      assert packet_index(packet, 36) == "1"
      # field :compliment
      assert packet_index(packet, 37) == "100"
      # field :size
      assert packet_index(packet, 38) == "17"
      # field :hero_level
      assert packet_index(packet, 39) == "54"
      # field :title_id
      assert packet_index(packet, 40) == "9407"
    end

    test "can serialize when no family is given" do
      family_attrs = %{
        family_id_rank: FamilyIdRankSubPacket.default(),
        family_name: nil,
        family_level: 0
      }

      packet = structure_to_iolist(in_mock(family_attrs))

      assert packet_index(packet, 26) == "-1"
      assert packet_index(packet, 27) == "-"
      assert packet_index(packet, 34) == "0"
    end
  end

  ## Helpers

  defp in_mock(attrs \\ %{}) do
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

    family_id_rank = %FamilyIdRankSubPacket{id: 124, rank: :head}
    weapon_upgrade = %ItemUpgradeRaritySubPacket{upgrade: 5, rarity: 6}
    armor_upgrade = %ItemUpgradeRaritySubPacket{upgrade: 7, rarity: 8}

    Map.merge(
      %InCharacterPacket{
        name: "admin",
        entity_id: 203_210,
        map_x: 19,
        map_y: 23,
        direction: :south,
        authority: :player,
        gender: :female,
        hair_style: :hair_style_d,
        hair_color: :pink_red,
        class: :martial_artist,
        equipments: equipments,
        hp_percent: 50,
        mp_percent: 60,
        is_sitting: false,
        group_id: 9999,
        fairy_move_type_id: 1,
        fairy_element: :light,
        fairy_morph: 42,
        spawn_effect: :falling,
        morph: 13,
        weapon_upgrade: weapon_upgrade,
        armor_upgrade: armor_upgrade,
        family_id_rank: family_id_rank,
        family_name: "ElvenGard",
        reputation_icon_id: 20,
        is_invisible: false,
        morph_upgrade: 15,
        faction: :neutral,
        morph_design: 8,
        level: 99,
        family_level: 20,
        family_icons: [true, false, true],
        is_arena_winner: true,
        compliment: 100,
        size: 17,
        hero_level: 54,
        title_id: 9407
      },
      attrs
    )
  end
end
