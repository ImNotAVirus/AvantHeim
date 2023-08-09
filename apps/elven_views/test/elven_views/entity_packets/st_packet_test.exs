defmodule ElvenViews.EntityPackets.StPacketTest do
  use PacketCase, async: true

  alias ElvenViews.EntityPackets.StPacket

  ## Tests

  describe "serialize/2" do
    test "can serialize a packet structure" do
      packet = structure_to_iolist(st_mock())

      assert is_list(packet)
      assert length(packet) == 9
      assert packet_index(packet, 0) == "st"
      assert packet_index(packet, 1) == "1"
      assert packet_index(packet, 2) == "11"
      assert packet_index(packet, 3) == "22"
      assert packet_index(packet, 4) == "33"
      assert packet_index(packet, 5) == "44"
      assert packet_index(packet, 6) == "55"
      assert packet_index(packet, 7) == "66"
      assert packet_index(packet, 8) == "77"
    end

    test "can serialize a packet with buffs" do
      # Single buff
      attrs = %{buffs: [111]}
      packet = structure_to_iolist(st_mock(attrs))

      assert length(packet) == 10
      assert packet_index(packet, 9) == "111"

      # Multiple buffs
      attrs = %{buffs: [111, 222, 333]}
      packet = structure_to_iolist(st_mock(attrs))

      assert length(packet) == 10
      assert packet_index(packet, 9) == "111 222 333"
    end
  end

  ## Helpers

  defp st_mock(attrs \\ %{}) do
    Map.merge(
      %StPacket{
        entity_type: :character,
        entity_id: 11,
        level: 22,
        hero_level: 33,
        hp_percent: 44,
        mp_percent: 55,
        hp: 66,
        mp: 77,
        buffs: []
      },
      attrs
    )
  end
end
