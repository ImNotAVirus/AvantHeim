defmodule ChannelEndpoint.Endpoint.UIViews do
  @moduledoc """
  TODO: Documentation
  """

  alias ChannelEndpoint.Endpoint.UIPackets.{Info, Scene}

  ## Public API

  @spec render(atom, any) :: any
  def render(:info, %{message: message}), do: %Info{message: message}
  def render(:scene, %{scene_id: scene_id}), do: %Scene{scene_id: scene_id}
end
