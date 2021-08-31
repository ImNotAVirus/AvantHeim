defmodule ChannelEndpoint.Endpoint.PlayerPackets.Pjoin do
  @moduledoc """
  TODO: Documentation

  Ex: #pjoin^3^1
  """

  use Core.SerializableStruct

  import ChannelEndpoint.GroupRequestEnums, only: [group_request_type: 2]

  alias __MODULE__

  @enforce_keys [:request_type, :id]
  defstruct @enforce_keys

  @type t :: %Pjoin{
          request_type: GroupRequestEnums.group_request_type_keys(),
          id: pos_integer
        }

  @impl true
  def serialize(%Pjoin{} = struct, _) do
    %Pjoin{
      request_type: request_type,
      id: id
    } = struct

    ["pjoin", group_request_type(request_type, :value), id]
  end
end
