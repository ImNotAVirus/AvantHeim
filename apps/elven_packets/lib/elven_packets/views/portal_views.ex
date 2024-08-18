defmodule ElvenPackets.Views.PortalViews do
  @moduledoc """
  TODO: ElvenPackets.Views.PortalViews
  """

  use ElvenGard.Network.View

  import ElvenPackets.View, only: [optional_param: 3, required_param: 2]

  alias ElvenPackets.Server.PortalPackets.Gp

  @impl true
  def render(:gp, args) do
    %Gp{
      source_x: required_param(args, :source_x),
      source_y: required_param(args, :source_y),
      map_id: required_param(args, :map_id),
      portal_type: required_param(args, :portal_type),
      portal_direction: optional_param(args, :portal_direction, :north),
      is_disabled: optional_param(args, :is_disabled, false)
    }
  end
end
