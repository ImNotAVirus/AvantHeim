defmodule ChannelEndpoint.Endpoint.PlayerViews do
  @moduledoc """
  TODO: Documentation
  """
  
  alias FakeData.Character
  alias ChannelEndpoint.Endpoint.PlayerPackets.{
    Tit
  }
  
  ## Public API

  @spec render(atom, any) :: any
  def render(:tit, %Character{} = character) do
    %Tit{class: character.class, name: character.name}
  end
end
