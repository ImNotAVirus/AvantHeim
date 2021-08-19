defmodule ChannelEndpoint.Endpoint.VisibilityPackets.InCharacter do
  @moduledoc """
  TODO: Documentation.

  FIXME: Some fields are probably wrong
  """

  use Core.SerializableStruct

  require DatabaseService.{EntityEnums, PlayerEnums}

  alias __MODULE__
  alias DatabaseService.{EntityEnums, PlayerEnums}

  @enforce_keys [
    :name,
    :character_id,
    :map_x,
    :map_y,
    :direction,
    :name_color_id,
    :gender,
    :hair_style,
    :hair_color,
    :class,
    :equipments,
    :hp_percent,
    :mp_percent,
    :is_sitting,
    :group_id,
    :fairy_move_type_id,
    :fairy_element,
    :fairy_morph,
    :morph,
    :weapon_upgrade,
    :armor_upgrade,
    :family_id,
    :family_rank,
    :family_name,
    :reputation_icon_id,
    :is_invisible,
    :morph_upgrade,
    :faction,
    :morph_design,
    :level,
    :family_level,
    :family_icon_ids,
    :is_arena_winner,
    :compliment,
    :hero_level,
    :size,
    :title_id
  ]
  defstruct @enforce_keys

  @type t :: %InCharacter{
          name: String.t(),
          character_id: pos_integer,
          map_x: non_neg_integer,
          map_y: non_neg_integer,
          direction: atom,
          name_color_id: non_neg_integer,
          gender: atom,
          hair_style: atom,
          hair_color: atom,
          class: atom,
          equipments: list,
          hp_percent: non_neg_integer,
          mp_percent: non_neg_integer,
          is_sitting: boolean,
          group_id: integer,
          fairy_move_type_id: non_neg_integer,
          fairy_element: atom,
          fairy_morph: non_neg_integer,
          morph: non_neg_integer,
          weapon_upgrade: non_neg_integer,
          armor_upgrade: non_neg_integer,
          family_id: integer,
          family_rank: atom,
          family_name: String.t() | nil,
          reputation_icon_id: integer,
          is_invisible: boolean,
          morph_upgrade: non_neg_integer,
          faction: atom,
          morph_design: non_neg_integer,
          level: non_neg_integer,
          family_level: non_neg_integer,
          family_icon_ids: list,
          is_arena_winner: boolean,
          compliment: non_neg_integer,
          hero_level: non_neg_integer,
          size: pos_integer,
          title_id: integer
        }

  @impl true
  def serialize(%InCharacter{} = struct, _) do
    %InCharacter{
      name: name,
      character_id: character_id,
      map_x: map_x,
      map_y: map_y,
      direction: direction,
      name_color_id: name_color_id,
      gender: gender,
      hair_style: hair_style,
      hair_color: hair_color,
      class: class,
      equipments: equipments,
      hp_percent: hp_percent,
      mp_percent: mp_percent,
      is_sitting: is_sitting,
      group_id: group_id,
      fairy_move_type_id: fairy_move_type_id,
      fairy_element: fairy_element,
      fairy_morph: fairy_morph,
      morph: morph,
      weapon_upgrade: weapon_upgrade,
      armor_upgrade: armor_upgrade,
      family_id: family_id,
      family_rank: family_rank,
      family_name: family_name,
      reputation_icon_id: reputation_icon_id,
      is_invisible: is_invisible,
      morph_upgrade: morph_upgrade,
      faction: faction,
      morph_design: morph_design,
      level: level,
      family_level: family_level,
      family_icon_ids: family_icon_ids,
      is_arena_winner: is_arena_winner,
      compliment: compliment,
      hero_level: hero_level,
      size: size,
      title_id: title_id
    } = struct

    vnum = "-"
    unknown1 = 0
    unknown2 = 0

    family_i18n =
      if family_id < 0, do: "-1", else: "#{family_id}.#{i18n_family_rank(family_rank)}"

    [
      "in",
      EntityEnums.entity_type(:character, :value),
      name,
      vnum,
      character_id,
      map_x,
      map_y,
      EntityEnums.direction_type(direction, :value),
      name_color_id,
      PlayerEnums.gender(gender, :value),
      PlayerEnums.hair_style(hair_style, :value),
      PlayerEnums.hair_color(hair_color, :value),
      PlayerEnums.character_class(class, :value),
      serialize_term(equipments, joiner: ".", as: :integer),
      hp_percent,
      mp_percent,
      is_sitting,
      group_id,
      fairy_move_type_id,
      EntityEnums.element_type(fairy_element, :value),
      unknown1,
      fairy_morph,
      unknown2,
      morph,
      weapon_upgrade,
      armor_upgrade,
      family_i18n,
      serialize_term(family_name, as: :string),
      reputation_icon_id,
      is_invisible,
      morph_upgrade,
      PlayerEnums.faction(faction, :value),
      morph_design,
      level,
      family_level,
      serialize_term(family_icon_ids, joiner: "|"),
      is_arena_winner,
      compliment,
      size,
      hero_level,
      title_id
    ]
  end

  ## Private functions

  defp i18n_family_rank(:head), do: 915
  defp i18n_family_rank(:deputy), do: 916
  defp i18n_family_rank(:keeper), do: 917
  defp i18n_family_rank(:member), do: 918
end
