defmodule ElvenPackets.Views.ChatViews do
  @moduledoc """
  TODO: ElvenPackets.Views.ChatViews
  """

  use ElvenGard.Network.View

  import ElvenPackets.View, only: [optional_param: 2, required_param: 2]

  alias ElvenPackets.Server.ChatPackets.{Bn, Say}

  @impl true
  def render(:bn, args) do
    id = required_param(args, :id)
    message = required_param(args, :message)

    %Bn{id: id, message: message}
  end

  def render(:say, args) do
    entity = required_param(args, :entity)
    message = required_param(args, :message)
    color = optional_param(args, :color)

    %Say{
      entity_type: GameService.entity_type(entity),
      entity_id: entity.id,
      color: color,
      message: message
    }
  end
end
