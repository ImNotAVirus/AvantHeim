defmodule GameService.EntityCase do
  use ExUnit.CaseTemplate

  alias ElvenGard.ECS.{Entity, Command}

  using _ do
    quote do
      import unquote(__MODULE__), only: [spawn_player: 0, spawn_player: 1]
    end
  end

  ## Helpers

  def spawn_player(attrs \\ []) do
    {:ok, {entity, _components}} =
      attrs
      |> Entity.entity_spec()
      |> Map.update!(:id, &{:player, &1})
      |> Command.spawn_entity()

    entity
  end
end
