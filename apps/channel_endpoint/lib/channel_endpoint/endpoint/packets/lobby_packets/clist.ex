defmodule ChannelEndpoint.Endpoint.LobbyPackets.Clist do
  @moduledoc """
  TODO: Documentation.
  """

  use Core.SerializableStruct

  require DatabaseService.PlayerEnums

  alias __MODULE__
  alias DatabaseService.PlayerEnums

  @enforce_keys [
    :slot,
    :name,
    :gender,
    :hair_style,
    :hair_color,
    :class,
    :level,
    :hero_level,
    :equipments,
    :job_level
  ]
  defstruct @enforce_keys ++ [design: 0, pets: []]

  @type t :: %Clist{}

  @impl true
  def serialize(%Clist{} = clist, _) do
    %Clist{
      slot: slot,
      name: name,
      gender: gender,
      hair_style: hair_style,
      hair_color: hair_color,
      class: class,
      level: level,
      hero_level: hero_level,
      job_level: job_level,
      equipments: equipments,
      design: design,
      pets: pets
    } = clist

    quest_completion = 1
    quest_part = 1

    gender_val = PlayerEnums.gender(gender, :value)
    hair_style_val = PlayerEnums.hair_style(hair_style, :value)
    hair_color_val = PlayerEnums.hair_color(hair_color, :value)
    class_val = PlayerEnums.character_class(class, :value)

    [
      "clist", slot, name, 0, gender_val, hair_style_val, hair_color_val, 0,
      class_val, level, hero_level, equipments, job_level, quest_completion,
      quest_part, pets, design
    ]
  end
end
