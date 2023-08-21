defmodule ElvenViews.UIPackets.ScenePacket do
  @moduledoc """
  TODO: Documentation.
  """

  use ElvenViews.SerializablePacket

  ## Packet definition

  defpacket "scene" do
    field :scene_id, :non_neg_integer
    field :cancellable, :boolean
  end
end
