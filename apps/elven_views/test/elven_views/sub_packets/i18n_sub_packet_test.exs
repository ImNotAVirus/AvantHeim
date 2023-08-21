defmodule ElvenViews.SubPackets.I18nSubPacketTest do
  use PacketCase, async: true

  alias ElvenViews.SubPackets.I18nSubPacket

  ## Tests

  describe "can serialize sub packet" do
    test "with no args" do
      family_id_rank = %I18nSubPacket{key: "ThankYouForUsingTheCuarryBank"}
      assert serialize_term(family_id_rank) == "2353 0"
    end

    test "with args" do
      family_id_rank = %I18nSubPacket{key: "BalanceBank", args: ["123", "456"]}
      assert serialize_term(family_id_rank) == "2345 3 123 456 0"
    end
  end
end
