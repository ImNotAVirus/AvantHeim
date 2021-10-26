defmodule ChannelEndpoint.Endpoint.UIPackets.Pidx do
  @moduledoc """
  TODO: Documentation.
  """

  use Core.SerializableStruct

  alias ChannelEndpoint.Endpoint.EntityPacket.PidxSubGroupMember

  alias __MODULE__

  @enforce_keys [:group_id, :sub_packet]
  defstruct @enforce_keys

  @type t :: %Pidx{
          group_id: neg_integer,
          sub_packet: [PidxSubGroupMember]
        }

  @impl true
  def serialize(%Pidx{} = struct, _) do
    %Pidx{
      group_id: group_id,
      sub_packet: sub_packet
    } = struct

    ["pidx", group_id, serialize_term(sub_packet, joiner: " ")]
  end
end
