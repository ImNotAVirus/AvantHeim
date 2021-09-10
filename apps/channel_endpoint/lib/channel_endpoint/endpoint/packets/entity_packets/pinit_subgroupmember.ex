defmodule ChannelEndpoint.Endpoint.EntityPacket.Pinit.SubGroupMember do
  alias DatabaseService.EntityEnums
  alias DatabaseService.PlayerEnums

  alias __MODULE__

  import DatabaseService.EntityEnums, only: [entity_type: 2]
  import DatabaseService.PlayerEnums, only: [gender: 2]

  @enforce_keys [
    :entity_type,
    :entity_id,
    :group_position,
    :level,
    :name,
    :unknow,
    :gender,
    :race,
    :morph,
    :hero_level
  ]
  defstruct @enforce_keys

  @type t :: %SubGroupMember{
          entity_type: EntityEnums.visual_type_keys(),
          entity_id: pos_integer,
          group_position: non_neg_integer,
          level: pos_integer,
          name: String.t(),
          unknow: non_neg_integer,
          gender: PlayerEnums.gender_keys(),
          race: non_neg_integer,
          morph: pos_integer,
          hero_level: pos_integer
        }

  ## Public API

  @spec parse(String.t(), keyword) :: {:ok, value :: [any], rest :: String.t()} | :error
  def parse(bin, opts) do
    sep = Keyword.fetch!(opts, :separator)

    case String.split(bin, sep, parts: 3) do
      [
        entity_type,
        entity_id,
        group_position,
        level,
        name,
        unknow,
        gender,
        race,
        morph,
        hero_level,
        rest
      ] ->
        {:ok,
         to_struct(
           entity_type,
           entity_id,
           group_position,
           level,
           name,
           unknow,
           gender,
           race,
           morph,
           hero_level
         ), rest}

      [
        entity_type,
        entity_id,
        group_position,
        level,
        name,
        unknow,
        gender,
        race,
        morph,
        hero_level
      ] ->
        {:ok,
         to_struct(
           entity_type,
           entity_id,
           group_position,
           level,
           name,
           unknow,
           gender,
           race,
           morph,
           hero_level
         ), ""}

      _ ->
        :error
    end
  end

  ## Private functions

  defp to_struct(
         entity_type,
         entity_id,
         group_position,
         level,
         name,
         unknow,
         gender,
         race,
         morph,
         hero_level
       ) do
    %SubGroupMember{
      entity_type: entity_type(entity_type, :value),
      entity_id: String.to_integer(entity_id),
      group_position: String.to_integer(group_position),
      level: String.to_integer(level),
      name: name,
      unknow: String.to_integer(unknow),
      gender: gender(gender, :value),
      race: String.to_integer(race),
      morph: String.to_integer(morph),
      hero_level: String.to_integer(hero_level)
    }
  end
end
