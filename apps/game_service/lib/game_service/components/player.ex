defmodule GameService.PlayerComponents do
  @moduledoc """
  TODO: Documentation for GameService.PlayerComponents
  """

  defmodule AccountComponent do
    use ElvenGard.ECS.Component, state: [:id, :username]
  end

  defmodule EndpointComponent do
    use ElvenGard.ECS.Component, state: [:pid]
  end

  defmodule PlayerComponent do
    use ElvenGard.ECS.Component, state: [:name, :gender, :class, :hair_color, :hair_style]
  end

  defmodule FactionComponent do
    use ElvenGard.ECS.Component, state: [:faction]
  end

  defmodule JobLevelComponent do
    use ElvenGard.ECS.Component, state: [:level, :xp]
  end

  defmodule HeroLevelComponent do
    use ElvenGard.ECS.Component, state: [:level, :xp]
  end

  defmodule CurrencyComponent do
    use ElvenGard.ECS.Component, state: [:gold, :bank_gold]
  end
end
