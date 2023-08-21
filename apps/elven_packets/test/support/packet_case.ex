defmodule ElvenPackets.PacketCase do
  use ExUnit.CaseTemplate

  ## Public API

  using do
    quote do
      import ElvenPackets.PacketCase, only: [serialize_packet: 1]
    end
  end

  ## Helpers

  def serialize_packet(%struct{} = packet), do: struct.serialize(packet)
end
