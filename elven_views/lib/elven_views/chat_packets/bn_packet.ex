defmodule ElvenViews.ChatPackets.BnPacket do
  @moduledoc """
  TODO: Documentation.
  """

  use ElvenCore.SerializableStruct

  alias __MODULE__

  @enforce_keys [:id, :message]
  defstruct @enforce_keys

  @type t :: %BnPacket{
          id: pos_integer,
          message: String.t()
        }

  @impl true
  def serialize(%BnPacket{} = struct, _) do
    %BnPacket{id: id, message: message} = struct
    ["bn", id, serialize_term(message, escape: true)]
  end
end
