defmodule ElvenPackets.ServerPackets.VisibilityPacketsTest do
  use ElvenPackets.PacketCase, async: true

  alias ElvenPackets.SubPackets.Equipment
  alias ElvenPackets.SubPackets.Item.UpgradeRarity
  alias ElvenPackets.SubPackets.Player.Family
  alias ElvenPackets.Server.VisibilityPackets.{InPlayer, Out}

  ## Tests

  describe "in" do
    test "can be serialized - player" do
      packet = in_player()
      assert {"in", params} = serialize_packet(packet)
      assert is_list(params)
      assert length(params) == 39

      # field :entity_type
      assert Enum.at(params, 0) == "1"
      # field :name
      assert Enum.at(params, 1) == "admin"
      # field :vnum
      assert Enum.at(params, 2) == "-"
      # field :entity_id
      assert Enum.at(params, 3) == "203210"
      # field :map_x
      assert Enum.at(params, 4) == "19"
      # field :map_y
      assert Enum.at(params, 5) == "23"
      # field :direction
      assert Enum.at(params, 6) == "2"
      # field :authority
      assert Enum.at(params, 7) == "0"
      # field :gender
      assert Enum.at(params, 8) == "1"
      # field :hair_style
      assert Enum.at(params, 9) == "3"
      # field :hair_color
      assert Enum.at(params, 10) == "9"
      # field :class
      assert Enum.at(params, 11) == "4"
      # field :equipments
      assert Enum.at(params, 12) == "339.4479.4983.4980.227.4131.4402.4401.-1.4443"
      # field :hp_percent
      assert Enum.at(params, 13) == "50"
      # field :mp_percent
      assert Enum.at(params, 14) == "60"
      # field :is_sitting
      assert Enum.at(params, 15) == "0"
      # field :group_id
      assert Enum.at(params, 16) == "9999"
      # field :fairy_move_type_id
      assert Enum.at(params, 17) == "1"
      # field :fairy_element
      assert Enum.at(params, 18) == "3"
      # field :unknown1
      assert Enum.at(params, 19) == "0"
      # field :fairy_morph
      assert Enum.at(params, 20) == "42"
      # field :spawn_effect
      assert Enum.at(params, 21) == "2"
      # field :morph
      assert Enum.at(params, 22) == "24"
      # field :weapon_upgrade
      assert Enum.at(params, 23) == "56"
      # field :armor_upgrade
      assert Enum.at(params, 24) == "78"
      # field :family
      assert Enum.at(params, 25) == "1.918 ElvenGard"
      # field :reputation_icon
      assert Enum.at(params, 26) == "32"
      # field :is_invisible
      assert Enum.at(params, 27) == "0"
      # field :morph_upgrade
      assert Enum.at(params, 28) == "15"
      # field :faction
      assert Enum.at(params, 29) == "0"
      # field :wings_design
      assert Enum.at(params, 30) == "28"
      # field :level
      assert Enum.at(params, 31) == "99"
      # field :family_level
      assert Enum.at(params, 32) == "20"
      # field :family_icons
      assert Enum.at(params, 33) == "1|0|1"
      # field :is_arena_winner
      assert Enum.at(params, 34) == "1"
      # field :compliment
      assert Enum.at(params, 35) == "100"
      # field :size
      assert Enum.at(params, 36) == "17"
      # field :hero_level
      assert Enum.at(params, 37) == "54"
      # field :title_id
      assert Enum.at(params, 38) == "9407"
    end
  end

  describe "out" do
    test "can be serialized" do
      packet = %Out{
        entity_type: :character,
        entity_id: 111
      }

      assert {"out", params} = serialize_packet(packet)
      assert is_list(params)
      assert length(params) == 2
      assert Enum.at(params, 0) == "1"
      assert Enum.at(params, 1) == "111"
    end
  end

  ## Helpers

  defp in_player() do
    equipments = %Equipment{
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

    family = %Family{id: 1, rank: :member, name: "ElvenGard"}
    weapon_upgrade = %UpgradeRarity{upgrade: 5, rarity: 6}
    armor_upgrade = %UpgradeRarity{upgrade: 7, rarity: 8}

    %InPlayer{
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
      morph: :demon_hunter,
      weapon_upgrade: weapon_upgrade,
      armor_upgrade: armor_upgrade,
      family: family,
      reputation_icon: :legendary_hero,
      is_invisible: false,
      morph_upgrade: 15,
      faction: :neutral,
      wings_design: :tree,
      level: 99,
      family_level: 20,
      family_icons: [true, false, true],
      is_arena_winner: true,
      compliment: 100,
      size: 17,
      hero_level: 54,
      title_id: 9407
    }
  end
end
