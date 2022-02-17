defmodule ChannelService.Endpoint.UIPackets.Cancel do
  @moduledoc """
  TODO: Documentation.
  """

  use ElvenCore.SerializableStruct

  alias __MODULE__

  # @enforce_keys [type: 0, caller_id: 0]
  defstruct type: 0, entity_id: 0

  @type t :: %Cancel{type: non_neg_integer, entity_id: non_neg_integer}

  @impl true
  def serialize(%Cancel{} = struct, _) do
    ["cancel", struct.type, struct.entity_id, "-1"]
  end
end
