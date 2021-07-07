defmodule ChannelEndpoint.Endpoint.ChatPackets.Bn do
  @moduledoc """
  TODO: Documentation.
  """

  use Core.SerializableStruct

  alias __MODULE__

  @enforce_keys [:id, :message]
  defstruct @enforce_keys

  @type t :: %Bn{
          id: pos_integer,
          message: String.t()
        }

  @impl true
  def serialize(%Bn{} = struct, _) do
    %Bn{id: id, message: message} = struct
    ["bn", id, serialize_term(message, escape: true)]
  end
end
