defmodule GameService.NpcBundle do
  @moduledoc """
  TODO: Documentation for GameService.NpcBundle
  """

  alias __MODULE__
  alias ElvenGard.ECS.{Component, Entity}
  alias GameService.EntityComponents, as: E
  alias GameService.NpcComponents, as: M

  ## NpcBundle structures (for outside use)

  @enforce_keys [
    :id,
    :npc,
    :position,
    :speed,
    :direction,
    :effect,
    :dialog,
    :quest,
    :shop
  ]
  defstruct @enforce_keys

  @typep component(module) :: module | :unset
  @typep maybe_component(module) :: component(module) | nil

  @type t :: %NpcBundle{
          id: pos_integer(),
          # Basics components
          npc: component(M.NpcComponent),
          position: component(E.PositionComponent),
          speed: component(E.SpeedComponent),
          direction: component(E.DirectionComponent),
          # Optional components
          effect: maybe_component(M.EffectComponent),
          dialog: maybe_component(M.DialogComponent),
          quest: maybe_component(M.QuestDialogComponent),
          shop: maybe_component(M.ShopComponent)
        }

  ## Public API

  @spec specs(map()) :: ElvenGard.ECS.Entity.spec()
  def specs(attrs) do
    id = Map.fetch!(attrs, :id)

    Entity.entity_spec(
      id: {:npc, id},
      components: [
        # Basics components
        {M.NpcComponent, npc_specs(attrs)},
        {E.PositionComponent, position_specs(attrs)},
        {E.SpeedComponent, speed_specs(attrs)},
        {E.DirectionComponent, direction_specs(attrs)},
        # Optional components
        {M.EffectComponent, effect_specs(attrs)},
        {M.DialogComponent, dialog_specs(attrs)},
        {M.QuestDialogComponent, quest_specs(attrs)}
      ]
    )
  end

  @doc """
  This function can be use to create a NpcBundle from an Entity an a list of components
  """
  @spec load(Entity.t(), [Component.t()]) :: t()
  def load(%Entity{id: {:npc, id}}, components) when is_list(components) do
    # mapping = Enum.group_by(components, & &1.__struct__)
    mapping = Map.new(components, &{&1.__struct__, &1})

    %NpcBundle{
      id: id,
      # Basics components
      npc: Map.fetch!(mapping, M.NpcComponent),
      position: Map.fetch!(mapping, E.PositionComponent),
      direction: Map.fetch!(mapping, E.DirectionComponent),
      # Optional components
      speed: Map.get(mapping, E.SpeedComponent),
      effect: Map.get(mapping, M.EffectComponent),
      dialog: Map.get(mapping, M.DialogComponent),
      quest: Map.get(mapping, M.QuestComponent),
      shop: Map.get(mapping, M.ShopComponent)
    }
  end

  @doc """
  This function can be use to create a NpcBundle from an Entity an a list of components

  Unlike `load/2`, you don't have to provide all components.
  Components not found will have the value `:unset`

  NOTE: You must verify that you have the required components in your system.
  """
  @spec preload(Entity.t(), [Component.t()]) :: t()
  def preload(%Entity{id: {:npc, id}}, components) when is_list(components) do
    # mapping = Enum.group_by(components, & &1.__struct__)
    mapping = Map.new(components, &{&1.__struct__, &1})

    %NpcBundle{
      id: id,
      # Basics components
      npc: Map.get(mapping, M.NpcComponent, :unset),
      position: Map.get(mapping, E.PositionComponent, :unset),
      direction: Map.get(mapping, E.DirectionComponent, :unset),
      # Optional components
      speed: Map.get(mapping, E.SpeedComponent, :unset),
      effect: Map.get(mapping, M.EffectComponent, :unset),
      dialog: Map.get(mapping, M.DialogComponent, :unset),
      quest: Map.get(mapping, M.QuestComponent, :unset),
      shop: Map.get(mapping, M.ShopComponent, :unset)
    }
  end

  ## Getters

  def vnum(%NpcBundle{} = npc) do
    case npc.npc do
      :unset -> raise ArgumentError, "you must fetch the Npc.NpcComponent first"
      npc -> npc.vnum
    end
  end

  def map_ref(%NpcBundle{} = npc) do
    case npc.position do
      :unset -> raise ArgumentError, "you must fetch the Entity.PositionComponent first"
      position -> position.map_ref
    end
  end

  def map_x(%NpcBundle{} = npc) do
    case npc.position do
      :unset -> raise ArgumentError, "you must fetch the Entity.PositionComponent first"
      position -> position.map_x
    end
  end

  def map_y(%NpcBundle{} = npc) do
    case npc.position do
      :unset -> raise ArgumentError, "you must fetch the Entity.PositionComponent first"
      position -> position.map_y
    end
  end

  def direction(%NpcBundle{} = npc) do
    case npc.direction do
      :unset -> raise ArgumentError, "you must fetch the Entity.DirectionComponent first"
      direction -> direction.value
    end
  end

  def dialog_id(%NpcBundle{} = npc) do
    case npc.quest do
      :unset -> raise ArgumentError, "you must fetch the Entity.QuestComponent first"
      quest -> quest.dialog_id
    end
  end

  def shop_name(%NpcBundle{} = npc) do
    case npc.shop do
      :unset -> raise ArgumentError, "you must fetch the Entity.ShopComponent first"
      shop -> shop.name
    end
  end

  def shop_type(%NpcBundle{} = npc) do
    case npc.shop do
      :unset -> raise ArgumentError, "you must fetch the Entity.ShopComponent first"
      shop -> shop.type
    end
  end

  def shop_tabs(%NpcBundle{} = npc) do
    case npc.shop do
      :unset -> raise ArgumentError, "you must fetch the Entity.ShopComponent first"
      shop -> shop.tabs
    end
  end

  ## Components specs

  defp npc_specs(%{vnum: vnum}) do
    [vnum: vnum]
  end

  defp position_specs(%{map_id: map_id, map_x: map_x, map_y: map_y} = attrs) do
    map_ref = Map.get(attrs, :map_ref, map_id)
    [map_id: map_id, map_ref: map_ref, map_x: map_x, map_y: map_y]
  end

  defp speed_specs(attrs) do
    # FIXME: Hardcoded value, now sure if it's the best place
    [value: Map.get(attrs, :speed, 10)]
  end

  defp direction_specs(attrs) do
    [value: Map.get(attrs, :direction, :south)]
  end

  defp effect_specs(attrs) do
    [vnum: get_in(attrs, [:effect, :vnum]), delay: get_in(attrs, [:effect, :delay])]
  end

  defp dialog_specs(attrs) do
    [id: Map.get(attrs, :dialog)]
  end

  defp quest_specs(attrs) do
    [id: Map.get(attrs, :quest_dialog)]
  end
end
