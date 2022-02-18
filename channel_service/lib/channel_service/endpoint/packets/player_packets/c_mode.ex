defmodule ChannelService.Endpoint.PlayerPackets.CMode do
  @moduledoc """
  TODO: Documentation.

  Character model
  """

  use ElvenCore.SerializableStruct

  import ElvenEnums.EntityEnums, only: [entity_type: 1]

  alias __MODULE__

  @enforce_keys [
    :entity_type,
    :entity_id,
    :morph,
    :morph_upgrade,
    :morph_design,
    :is_arena_winner,
    :size,
    :item_morph
  ]
  defstruct @enforce_keys

  @type t :: %CMode{
          entity_type: atom,
          entity_id: pos_integer,
          morph: non_neg_integer,
          morph_upgrade: non_neg_integer,
          morph_design: non_neg_integer,
          is_arena_winner: boolean,
          size: pos_integer,
          item_morph: non_neg_integer
        }

  @impl true
  def serialize(%CMode{} = struct, _) do
    %CMode{
      entity_type: entity_type,
      entity_id: entity_id,
      morph: morph,
      morph_upgrade: morph_upgrade,
      morph_design: morph_design,
      is_arena_winner: is_arena_winner,
      size: size,
      item_morph: item_morph
    } = struct

    [
      "c_mode",
      entity_type(entity_type),
      entity_id,
      morph,
      morph_upgrade,
      morph_design,
      is_arena_winner,
      size,
      item_morph
    ]
  end
end
