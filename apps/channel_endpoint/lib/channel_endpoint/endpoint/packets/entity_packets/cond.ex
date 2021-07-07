defmodule ChannelEndpoint.Endpoint.EntityPackets.Cond do
  @moduledoc """
  TODO: Documentation.
  """

  use Core.SerializableStruct
  
  import DatabaseService.EntityEnums, only: [entity_type: 1]

  alias __MODULE__

  @enforce_keys [:entity_type, :entity_id, :no_attack, :no_move, :speed]
  defstruct @enforce_keys

  @type t :: %Cond{
    entity_type: atom,
    entity_id: pos_integer,
    no_attack: boolean,
    no_move: boolean,
    speed: non_neg_integer,
  }

  @impl true
  def serialize(%Cond{} = struct, _) do
    %Cond{
      entity_type: entity_type_atom,
      entity_id: entity_id,
      no_attack: no_attack,
      no_move: no_move,
      speed: speed
    } = struct
    
    ["cond", entity_type(entity_type_atom), entity_id, no_attack, no_move, speed]
  end
end
