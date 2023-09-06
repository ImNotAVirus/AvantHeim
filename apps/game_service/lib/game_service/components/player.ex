defmodule GameService.PlayerComponents do
  @moduledoc """
  TODO: Documentation for GameService.PlayerComponents
  """

  defmodule PlayerComponent do
    use ElvenGard.ECS.Component,
      state: [
        :account_id,
        :name,
        :gender,
        :class,
        :hair_color,
        :hair_style
      ]
  end

  defmodule EndpoindComponent do
    use ElvenGard.ECS.Component, state: [:pid]
  end

  defmodule FactionComponent do
    use ElvenGard.ECS.Component, state: [:value]
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
