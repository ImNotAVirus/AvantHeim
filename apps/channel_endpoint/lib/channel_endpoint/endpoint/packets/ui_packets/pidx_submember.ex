defmodule ChannelEndpoint.Endpoint.UIPackets.PidxSubGroupMember do
  alias __MODULE__

  use Core.SerializableStruct

  @enforce_keys [
    :is_grouped,
    :entity_id
  ]
  defstruct @enforce_keys

  @type t :: %PidxSubGroupMember{
          is_grouped: boolean,
          entity_id: pos_integer
        }

  ## Public API

  @impl true
  def serialize(%PidxSubGroupMember{} = struct, _) do
    %PidxSubGroupMember{
      is_grouped: is_grouped,
      entity_id: entity_id
    } = struct

    serialize_term(
      [
        is_grouped,
        entity_id
      ],
      joiner: "."
    )
  end
end
