defmodule ElvenViews.PlayerPackets.CInfoPacketTest do
  use PacketCase, async: true

  alias ElvenViews.PlayerPackets.CInfoPacket
  alias ElvenViews.PlayerPackets.CInfo.FamilyIdRankSubPacket

  ## Tests

  describe "serialize/2" do
    test "can serialize a packet structure" do
      packet = structure_to_iolist(c_info_mock())

      assert is_list(packet)
      assert length(packet) == 20
      assert packet_index(packet, 0) == "c_info"

      # field :name
      assert packet_index(packet, 1) == "DarkyZ"
      # field :vnum
      assert packet_index(packet, 2) == "-"
      # field :group_id
      assert packet_index(packet, 3) == "221"
      # field :family_id_rank
      assert packet_index(packet, 4) == "331.916"
      # field :family_name
      assert packet_index(packet, 5) == "AvantHeim"
      # field :character_id
      assert packet_index(packet, 6) == "4615"
      # field :name_color_id
      assert packet_index(packet, 7) == "10"
      # field :gender
      assert packet_index(packet, 8) == "0"
      # field :hair_style
      assert packet_index(packet, 9) == "3"
      # field :hair_color
      assert packet_index(packet, 10) == "5"
      # field :class
      assert packet_index(packet, 11) == "4"
      # field :reputation_icon_id
      assert packet_index(packet, 12) == "80"
      # field :compliment
      assert packet_index(packet, 13) == "100"
      # field :morph
      assert packet_index(packet, 14) == "1024"
      # field :is_invisible
      assert packet_index(packet, 15) == "0"
      # field :family_level
      assert packet_index(packet, 16) == "12"
      # field :morph_upgrade
      assert packet_index(packet, 17) == "15"
      # field :morph_design
      assert packet_index(packet, 18) == "16"
      # field :is_arena_winner
      assert packet_index(packet, 19) == "1"
    end

    test "can serialize when no family is given" do
      family_attrs = %{
        family_id_rank: FamilyIdRankSubPacket.default(),
        family_name: nil,
        family_level: 0
      }

      packet = structure_to_iolist(c_info_mock(family_attrs))

      assert packet_index(packet, 4) == "-1"
      assert packet_index(packet, 5) == "-"
      assert packet_index(packet, 16) == "0"
    end

    test "can serialize family rank i18n" do
      family_attrs = %{family_id_rank: %FamilyIdRankSubPacket{id: 123, rank: :head}}
      packet = structure_to_iolist(c_info_mock(family_attrs))
      assert packet_index(packet, 4) == "123.915"

      family_attrs = %{family_id_rank: %FamilyIdRankSubPacket{id: 123, rank: :deputy}}
      packet = structure_to_iolist(c_info_mock(family_attrs))
      assert packet_index(packet, 4) == "123.916"

      family_attrs = %{family_id_rank: %FamilyIdRankSubPacket{id: 123, rank: :keeper}}
      packet = structure_to_iolist(c_info_mock(family_attrs))
      assert packet_index(packet, 4) == "123.917"

      family_attrs = %{family_id_rank: %FamilyIdRankSubPacket{id: 123, rank: :member}}
      packet = structure_to_iolist(c_info_mock(family_attrs))
      assert packet_index(packet, 4) == "123.918"
    end
  end

  ## Helpers

  defp c_info_mock(attrs \\ %{}) do
    family_id_rank = %FamilyIdRankSubPacket{id: 331, rank: :deputy}

    Map.merge(
      %CInfoPacket{
        character_id: 4615,
        name: "DarkyZ",
        group_id: 221,
        family_id_rank: family_id_rank,
        family_name: "AvantHeim",
        name_color_id: 10,
        gender: :male,
        hair_style: :hair_style_d,
        hair_color: :brown,
        class: :martial_artist,
        reputation_icon_id: 80,
        compliment: 100,
        morph: 1024,
        is_invisible: false,
        family_level: 12,
        morph_upgrade: 15,
        morph_design: 16,
        is_arena_winner: true
      },
      attrs
    )
  end
end
