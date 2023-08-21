defmodule ElvenViews.SubPackets.EquipmentSubPacketTest do
  use PacketCase, async: true

  alias ElvenViews.SubPackets.EquipmentSubPacket

  ## Tests

  test "can serialize sub packet" do
    equipment = %EquipmentSubPacket{
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

    assert serialize_term(equipment) == "1.2.3.4.5.6.7.8.9.10"
  end

  test "can serialize sub packet with nil values" do
    equipment = %EquipmentSubPacket{
      hat: nil,
      armor: nil,
      main_weapon: nil,
      secondary_weapon: nil,
      mask: nil,
      fairy: nil,
      costume_suit: nil,
      costume_hat: nil,
      weapon_skin: nil,
      wings_skin: nil
    }

    assert serialize_term(equipment) == empty_equipment(10)
  end

  ## Private functions

  defp empty_equipment(length) do
    ["-1"] |> Stream.cycle() |> Enum.take(length) |> Enum.join(".")
  end
end
