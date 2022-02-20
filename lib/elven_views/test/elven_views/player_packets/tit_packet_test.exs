defmodule ElvenViews.PlayerPackets.TitPacketTest do
  use PacketCase, async: true

  alias ElvenViews.PlayerPackets.TitPacket

  ## Tests

  describe "serialize/2" do
    test "can serialize a packet structure" do
      packet = structure_to_iolist(tit_mock())

      assert is_list(packet)
      assert length(packet) == 3
      assert packet_index(packet, 0) == "tit"
      assert packet_index(packet, 1) == "39"
      assert packet_index(packet, 2) == "DarkyZ"
    end

    test "can serialize i18n for class packet" do
      class_attr = %{class: :adventurer}
      packet = structure_to_iolist(tit_mock(class_attr))
      assert packet_index(packet, 1) == "35"

      class_attr = %{class: :swordman}
      packet = structure_to_iolist(tit_mock(class_attr))
      assert packet_index(packet, 1) == "36"

      class_attr = %{class: :archer}
      packet = structure_to_iolist(tit_mock(class_attr))
      assert packet_index(packet, 1) == "37"

      class_attr = %{class: :magician}
      packet = structure_to_iolist(tit_mock(class_attr))
      assert packet_index(packet, 1) == "38"

      class_attr = %{class: :martial_artist}
      packet = structure_to_iolist(tit_mock(class_attr))
      assert packet_index(packet, 1) == "39"
    end
  end

  ## Helpers

  defp tit_mock(attrs \\ %{}) do
    Map.merge(
      %TitPacket{
        class: :martial_artist,
        name: "DarkyZ"
      },
      attrs
    )
  end
end
