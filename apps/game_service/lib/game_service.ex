defmodule GameService do
  @moduledoc """
  Documentation for `GameService`.
  """

  alias GameService.PlayerEntity

  def entity_type(%PlayerEntity{}), do: :character
  def entity_id(%PlayerEntity{id: id}), do: id
end
