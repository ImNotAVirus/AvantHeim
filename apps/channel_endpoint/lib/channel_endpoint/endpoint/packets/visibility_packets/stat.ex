defmodule ChannelEndpoint.Endpoint.VisibilityPackets.Stat do
  @moduledoc """
  TODO: Documentation.
  """

  use Core.SerializableStruct

  alias __MODULE__

  @enforce_keys [:hp, :hp_maximum, :mp, :mp_maximum]
  defstruct @enforce_keys

  @type t :: %Stat{
          hp: pos_integer,
          hp_maximum: pos_integer,
          mp: pos_integer,
          mp_maximum: pos_integer
        }

  @impl true
  def serialize(%Stat{} = struct, _) do
    %Stat{
      hp: hp,
      hp_maximum: hp_maximum,
      mp: mp,
      mp_maximum: mp_maximum
    } = struct

    ["stat", hp, hp_maximum, mp, mp_maximum, 0, 0]
  end
end
