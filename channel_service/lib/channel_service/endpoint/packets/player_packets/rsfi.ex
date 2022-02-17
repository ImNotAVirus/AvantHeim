defmodule ChannelService.Endpoint.PlayerPackets.Rsfi do
  @moduledoc """
  TODO: Documentation.
  """

  use ElvenCore.SerializableStruct

  alias __MODULE__

  defstruct act: 1, act_part: 1, ts: 0, ts_max: 0

  @type t :: %Rsfi{
          act: non_neg_integer,
          act_part: non_neg_integer,
          ts: non_neg_integer,
          ts_max: non_neg_integer
        }

  @impl true
  def serialize(%Rsfi{} = struct, _) do
    %Rsfi{
      act: act,
      act_part: act_part,
      ts: ts,
      ts_max: ts_max
    } = struct

    ["rsfi", act, act_part, 0, 0, ts, ts_max]
  end
end
