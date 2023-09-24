defmodule ElvenPackets.Views.ChatViews do
  @moduledoc """
  TODO: ElvenPackets.Views.ChatViews
  """

  use ElvenGard.Network.View

  import ElvenPackets.View, only: [optional_param: 2, required_param: 2]

  alias ElvenPackets.Server.ChatPackets.{Bn, Say}

  @impl true
  def render(:bn, args) do
    %Bn{
      id: required_param(args, :id),
      message: required_param(args, :message)
    }
  end

  def render(:say, args) do
    %Say{
      entity_type: required_param(args, :entity_type),
      entity_id: required_param(args, :entity_id),
      color: optional_param(args, :color),
      message: required_param(args, :message)
    }
  end
end
