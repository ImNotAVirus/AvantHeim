defmodule ElvenViews.PlayerPackets.LevPacket do
  @moduledoc """
  TODO: Documentation.
  """

  use ElvenViews.SerializablePacket

  ## Packet definition

  defpacket "lev" do
    field :level, :non_neg_integer
    field :level_xp, :non_neg_integer
    field :job_level, :non_neg_integer
    field :job_level_xp, :non_neg_integer
    field :level_xp_max, :non_neg_integer
    field :job_level_xp_max, :non_neg_integer
    field :reputation, :integer
    field :cp, :non_neg_integer
    field :hero_level_xp, :non_neg_integer
    field :hero_level, :non_neg_integer
    field :hero_level_xp_max, :non_neg_integer
    field :unknown, :integer, default: 0
  end
end
