defmodule ChannelEndpoint.Endpoint.UIPackets.Scene do
  @moduledoc """
  TODO: Documentation.
  """

  use Core.SerializableStruct

  alias __MODULE__

  @enforce_keys [:scene_id]
  defstruct @enforce_keys ++ [cancellable: true]

  @type t :: %Scene{
          scene_id: String.t(),
          cancellable: boolean
        }

  @impl true
  def serialize(%Scene{} = struct, _) do
    %Scene{scene_id: scene_id, cancellable: cancellable} = struct
    ["scene", scene_id, cancellable]
  end
end
