defmodule ChannelEndpoint.Endpoint.PlayerViews do
  @moduledoc """
  TODO: Documentation
  """

  alias FakeData.Character

  alias ChannelEndpoint.Endpoint.PlayerPackets.{
    Fd,
    Fs,
    Rsfi,
    Tit
  }

  ## Public API

  @spec render(atom, any) :: any
  def render(:rsfi, %Character{}), do: %Rsfi{}

  def render(:tit, %Character{} = character) do
    %Tit{class: character.class, name: character.name}
  end

  def render(:fs, %Character{} = character) do
    %Fs{faction: character.faction}
  end

  def render(:fd, %Character{} = character) do
    %Fd{
      reputation: FakeData.reputation(character_id: character.id),
      reputation_icon_id: FakeData.reputation_icon_id(character_id: character.id),
      dignity: FakeData.dignity(character_id: character.id),
      dignity_icon_id: FakeData.dignity_icon_id(character_id: character.id)
    }
  end
end
