defmodule PacketCase do
  use ExUnit.CaseTemplate

  setup do
    # This will run before each test that uses this case
    :ok
  end

  using do
    quote do
      def serialize(packet, opts \\ []) do
        packet.__struct__.serialize(packet, opts)
      end

      def packet_index(packet, index) do
        Enum.at(packet, index)
      end
    end
  end
end
