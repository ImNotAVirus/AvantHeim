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
    use ElvenGard.ECS.Component, state: [:level, :xp]
  end

  defmodule SpeedComponent do
    use ElvenGard.ECS.Component, state: [:value]
  end

  defmodule DirectionComponent do
    use ElvenGard.ECS.Component, state: [:value]
  end

  defmodule SittingComponent do
    use ElvenGard.ECS.Component, state: [:value]
  end
end
