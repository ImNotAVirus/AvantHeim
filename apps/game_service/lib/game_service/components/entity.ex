defmodule GameService.EntityComponents do
  @moduledoc """
  TODO: Documentation for GameService.EntityComponents
  """

  defmodule PositionComponent do
    use ElvenGard.ECS.Component, state: [:map_id, :map_ref, :map_x, :map_y]

    ## Helpers

    def map_instance?(%__MODULE__{map_ref: map_ref}), do: is_reference(map_ref)
  end

  defmodule LevelComponent do
    use ElvenGard.ECS.Component, state: [:value, :xp, :xp_max]
  end

  defmodule SpeedComponent do
    use ElvenGard.ECS.Component, state: [:value]
  end

  defmodule DirectionComponent do
    use ElvenGard.ECS.Component, state: [:value]
  end

  defmodule CombatComponent do
    use ElvenGard.ECS.Component,
      state: [
        :hp,
        :hp_max,
        :mp,
        :mp_max
      ]
  end

  defmodule SittingComponent do
    use ElvenGard.ECS.Component, state: []
  end

  defmodule InvisibilityComponent do
    use ElvenGard.ECS.Component, state: []
  end

  defmodule CannotAttackComponent do
    use ElvenGard.ECS.Component, state: []
  end

  defmodule CannotMoveComponent do
    use ElvenGard.ECS.Component, state: []
  end
end
