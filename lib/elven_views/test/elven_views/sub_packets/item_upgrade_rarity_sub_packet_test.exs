defmodule ElvenViews.SubPackets.ItemUpgradeRaritySubPacketTest do
  use PacketCase, async: true

  alias ElvenViews.SubPackets.ItemUpgradeRaritySubPacket

  ## Tests

  test "can serialize sub packet" do
    family_id_rank = %ItemUpgradeRaritySubPacket{upgrade: 4, rarity: 5}
    assert serialize_term(family_id_rank) == "45"
  end
end
