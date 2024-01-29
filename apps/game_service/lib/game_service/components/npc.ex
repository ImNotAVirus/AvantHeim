defmodule GameService.NpcComponents do
  @moduledoc """
  TODO: Documentation for GameService.NpcComponents
  """

  defmodule NpcComponent do
    use ElvenGard.ECS.Component, state: [:vnum]
  end

  defmodule DialogComponent do
    use ElvenGard.ECS.Component, state: [:id]
  end

  defmodule EffectComponent do
    use ElvenGard.ECS.Component, state: [:vnum, :delay]
  end

  defmodule QuestDialogComponent do
    use ElvenGard.ECS.Component, state: [:id]
  end

  defmodule ShopComponent do
    use ElvenGard.ECS.Component, state: [:name, :menu, :type]
  end

  defmodule MenuComponent do
    use ElvenGard.ECS.Component, state: [:type]
  end

  defmodule InventoryComponent do
    use ElvenGard.ECS.Component, state: [:tabs]
  end
end
