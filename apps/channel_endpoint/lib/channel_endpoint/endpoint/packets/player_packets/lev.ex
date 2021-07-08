defmodule ChannelEndpoint.Endpoint.PlayerPackets.Lev do
  @moduledoc """
  TODO: Documentation.
  """

  use Core.SerializableStruct

  alias __MODULE__

  @enforce_keys [
    :level,
    :level_xp,
    :level_xp_max,
    :job_level,
    :job_level_xp,
    :job_level_xp_max,
    :hero_level,
    :hero_level_xp,
    :hero_level_xp_max,
    :reputation,
    :cp
  ]
  defstruct @enforce_keys

  @type t :: %Lev{
          level: non_neg_integer,
          level_xp: non_neg_integer,
          level_xp_max: non_neg_integer,
          job_level: non_neg_integer,
          job_level_xp: non_neg_integer,
          job_level_xp_max: non_neg_integer,
          hero_level: non_neg_integer,
          hero_level_xp: non_neg_integer,
          hero_level_xp_max: non_neg_integer,
          reputation: integer,
          cp: non_neg_integer
        }

  @impl true
  def serialize(%Lev{} = struct, _) do
    %Lev{
      level: level,
      level_xp: level_xp,
      job_level: job_level,
      job_level_xp: job_level_xp,
      level_xp_max: level_xp_max,
      job_level_xp_max: job_level_xp_max,
      reputation: reputation,
      cp: cp,
      hero_level_xp: hero_level_xp,
      hero_level: hero_level,
      hero_level_xp_max: hero_level_xp_max
    } = struct

    [
      "lev",
      level,
      level_xp,
      job_level,
      job_level_xp,
      level_xp_max,
      job_level_xp_max,
      reputation,
      cp,
      hero_level_xp,
      hero_level,
      hero_level_xp_max,
      0
    ]
  end
end
