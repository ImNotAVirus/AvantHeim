defmodule ElvenViews.ChatPackets.SayPacketTest do
  use ExUnit.Case, async: true

  require ElvenViews.ChatPackets.SayEnums
  require DatabaseService.EntityEnums

  alias ElvenViews.ChatPackets.SayPacket
  alias ElvenViews.ChatPackets.SayEnums
  alias DatabaseService.EntityEnums

  @entity_id 1
  @message "This is a message for the SayPacket"
  @entity_type :character
  @color :default

  ## Tests

  describe "serialize/2" do
    test "can serialize entity types" do
      Enum.each(EntityEnums.entity_type(:__enumerators__), fn {key, value} ->
        color_value = SayEnums.color_type(@color)
        assert ["say", ^value, @entity_id, ^color_value, @message] = serialize_say(key, @color)
      end)
    end

    test "can serialize colors" do
      Enum.each(SayEnums.color_type(:__enumerators__), fn {key, value} ->
        entity_type_value = EntityEnums.entity_type(@entity_type)

        assert ["say", ^entity_type_value, @entity_id, ^value, @message] =
                 serialize_say(@entity_type, key)
      end)
    end
  end

  ## Helpers

  defp serialize_say(entity_type, color) do
    %SayPacket{
      entity_type: entity_type,
      entity_id: @entity_id,
      color: color,
      message: @message
    }
    |> SayPacket.serialize([])
  end
end
