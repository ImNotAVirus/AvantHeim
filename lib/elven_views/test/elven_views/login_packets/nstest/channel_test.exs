defmodule ElvenViews.LoginPackets.NsTeST.ChannelTest do
  use ExUnit.Case, async: true

  alias ElvenViews.LoginPackets.NsTeST.Channel

  ## Tests

  describe "serialize/2" do
    test "can serialize the structure" do
      id = 1
      world_id = 2
      world_name = "ElvenGard"
      ip = "127.0.0.1"
      port = 4000
      population = 3

      channel = %Channel{
        id: id,
        world_id: world_id,
        world_name: world_name,
        ip: ip,
        port: port,
        population: population
      }

      expected = "#{ip}:#{port}:#{population}:#{world_id}.#{id}.#{world_name}"

      assert Channel.serialize(channel, []) == expected
    end
  end
end
