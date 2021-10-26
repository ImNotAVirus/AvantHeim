defmodule ChannelEndpoint.Endpoint.UIPackets.PinitEmptyGroup do
  alias __MODULE__

  use Core.SerializableStruct

  @enforce_keys [
    :unknow
  ]
  defstruct @enforce_keys

  @type t :: %PinitEmptyGroup{
          unknow: non_neg_integer
        }

  ## Public API

  @impl true
  def serialize(%PinitEmptyGroup{} = struct, _) do
    %PinitEmptyGroup{
      unknow: unknow
    } = struct

    ["pinit", unknow]
  end
end
