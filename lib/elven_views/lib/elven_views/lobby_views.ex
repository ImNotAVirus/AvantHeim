defmodule ElvenViews.LobbyViews do
  @moduledoc """
  TODO: Documentation
  """

  use ElvenViews

  alias ElvenViews.SubPackets.EquipmentSubPacket

  alias ElvenViews.LobbyPackets.{
    ClistEndPacket,
    ClistPacket,
    ClistStartPacket,
    OkPacket
  }

  ## Public API

  @impl true
  def render(:clist_start, _args), do: %ClistStartPacket{}
  def render(:clist_end, _args), do: %ClistEndPacket{}
  def render(:ok, _args), do: %OkPacket{}

  def render(:clist, args) do
    character = required_param(args, :character)
    equipments = required_param(args, :equipments)
    pets = optional_param(args, :pets, [])
    design = optional_param(args, :pets, 0)

    [
      hat,
      armor,
      main_weapon,
      secondary_weapon,
      mask,
      fairy,
      costume_suit,
      costume_hat,
      weapon_skin,
      wings_skin
    ] = equipments

    additional_params = %{
      equipments: %EquipmentSubPacket{
        hat: hat,
        armor: armor,
        main_weapon: main_weapon,
        secondary_weapon: secondary_weapon,
        mask: mask,
        fairy: fairy,
        costume_suit: costume_suit,
        costume_hat: costume_hat,
        weapon_skin: weapon_skin,
        wings_skin: wings_skin
      },
      pets: pets,
      design: design
    }

    character
    |> Map.take(~w(slot name gender hair_style hair_color class level hero_level job_level)a)
    |> Map.merge(additional_params)
    |> then(&struct!(ClistPacket, &1))
  end
end
