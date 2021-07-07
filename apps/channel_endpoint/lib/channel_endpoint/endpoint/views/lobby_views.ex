defmodule ChannelEndpoint.Endpoint.LobbyViews do
  @moduledoc """
  TODO: Documentation
  """

  alias DatabaseService.Players.Character

  alias ChannelEndpoint.Endpoint.LobbyPackets.{
    ClistStart,
    ClistEnd,
    Clist,
    Ok
  }

  ## Public API

  @spec render(atom, any) :: any
  def render(:ok, _), do: %Ok{}
  def render(:clist_start, _), do: %ClistStart{}
  def render(:clist_end, _), do: %ClistEnd{}

  def render(:clist, %Character{id: character_id} = character) do
    equipments = FakeData.equipments(character_id: character_id)

    fields =
      character
      |> Map.take(~w(slot name gender hair_style hair_color class level hero_level job_level)a)
      |> Map.put(:equipments, equipments)

    struct!(Clist, fields)
  end
end
