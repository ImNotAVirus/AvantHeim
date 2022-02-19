defmodule ElvenViews.SubPackets.FamilyIdRankSubPacketTest do
  use PacketCase, async: true

  alias ElvenViews.SubPackets.FamilyIdRankSubPacket

  ## Tests

  test "can serialize family rank i18n" do
    family_id_rank = %FamilyIdRankSubPacket{id: 123, rank: :head}
    assert serialize_term(family_id_rank) == "123.915"

    family_id_rank = %FamilyIdRankSubPacket{id: 123, rank: :deputy}
    assert serialize_term(family_id_rank) == "123.916"

    family_id_rank = %FamilyIdRankSubPacket{id: 123, rank: :keeper}
    assert serialize_term(family_id_rank) == "123.917"

    family_id_rank = %FamilyIdRankSubPacket{id: 123, rank: :member}
    assert serialize_term(family_id_rank) == "123.918"
  end
end
