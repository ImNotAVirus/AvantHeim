defmodule ChannelEndpoint.Endpoint.UIPackets.Pinit do
  @moduledoc """
  TODO: Documentation.
  """

  use Core.SerializableStruct

  alias ChannelEndpoint.Endpoint.EntityPacket.Pinit.SubGroupMember

  alias __MODULE__

  @enforce_keys [:group_size, :members]
  defstruct @enforce_keys

  @type t :: %Pinit{
          group_size: pos_integer,
          members: [SubGroupMember]
        }

  @impl true
  def serialize(%Pinit{} = struct, _) do
    %Pinit{
      group_size: group_size,
      members: members
    } = struct

    ["pinit", group_size, serialize_term(members, joiner: " ")]
  end
end
