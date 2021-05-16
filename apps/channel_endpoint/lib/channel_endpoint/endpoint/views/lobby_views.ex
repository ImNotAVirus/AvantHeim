defmodule ChannelEndpoint.Endpoint.LobbyViews do
  @moduledoc """
  TODO: Documentation
  """

  alias DatabaseService.Players.Character

  alias ChannelEndpoint.Endpoint.{
    ClistStartPacket,
    ClistEndPacket,
    ClistPacket
  }

  ## Public API

  @spec render(atom, any) :: any
  def render(:clist_start, _), do: %ClistStartPacket{}
  def render(:clist_end, _), do: %ClistEndPacket{}

  def render(:clist, %Character{} = character) do
    # TODO: Get it from DB
    equipments = [nil] |> Stream.cycle() |> Enum.take(10)

    fields =
      character
      |> Map.take(~w(slot name gender hair_style hair_color class level hero_level job_level)a)
      |> Map.put(:equipments, equipments)

    struct!(ClistPacket, fields)
  end
end
