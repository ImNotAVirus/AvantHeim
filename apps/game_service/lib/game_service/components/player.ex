defmodule GameService.PlayerComponents do
  @moduledoc """
  TODO: Documentation for GameService.PlayerComponents
  """

  defmodule AccountComponent do
    use ElvenGard.ECS.Component, state: [:id, :username, :authority]
  end

  defmodule EndpointComponent do
    use ElvenGard.ECS.Component, state: [:pid]
  end

  defmodule PlayerComponent do
    use ElvenGard.ECS.Component, state: [:name, :gender, :class, :hair_color, :hair_style]
  end

  defmodule FactionComponent do
    use ElvenGard.ECS.Component, state: [:value]
  end

  defmodule JobLevelComponent do
    use ElvenGard.ECS.Component, state: [:value, :xp, :xp_max]
  end

  defmodule HeroLevelComponent do
    use ElvenGard.ECS.Component, state: [:value, :xp, :xp_max]
  end

  defmodule GoldComponent do
    use ElvenGard.ECS.Component, state: [:value]
  end

  defmodule BankComponent do
    use ElvenGard.ECS.Component, state: [:gold, :rank, :tax]
  end

  defmodule SpecialistComponent do
    use ElvenGard.ECS.Component, state: [:type, :upgrade, :wings_design]
  end

  defmodule SizeComponent do
    use ElvenGard.ECS.Component, state: [:value]
  end

  defmodule GroupComponent do
    use ElvenGard.ECS.Component, state: [:id]
  end

  defmodule FamilyComponent do
    use ElvenGard.ECS.Component, state: [:id, :name, :rank, :level, :icons]
  end

  defmodule ReputationComponent do
    use ElvenGard.ECS.Component,
      state: [:dignity, :dignity_icon, :reputation, :reputation_icon, :compliment]
  end

  defmodule TitleComponent do
    use ElvenGard.ECS.Component, state: [:id]
  end

  defmodule FairyComponent do
    use ElvenGard.ECS.Component, state: [:type, :move_type, :element]
  end

  defmodule ArenaWinnerComponent do
    use ElvenGard.ECS.Component, state: []
  end
end
