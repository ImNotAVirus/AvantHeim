defmodule ChannelEndpoint.Endpoint.UIViews do
  @moduledoc """
  TODO: Documentation
  """

  alias CachingService.Player.Character
  alias ChannelEndpoint.Endpoint.UIPackets.{Cancel, Info, Scene, Gold, Gb}

  ## Public API

  @spec render(atom, any) :: any
  def render(:info, %{message: message}), do: %Info{message: message}
  def render(:scene, %{scene_id: scene_id}), do: %Scene{scene_id: scene_id}

  def render(:cancel, %{type: type, entity: %Character{id: id}}) do
    %Cancel{type: type, entity_id: id}
  end

  def render(:gold, %Character{} = character) do
    %Gold{gold: character.gold, bank_gold: character.bank_gold}
  end

  # TODO : Bank rank | tax | bank_action_type
  def render(:gb, %Character{} = character) do
    %Gb{
      bank_action_type: 0,
      gold_bank: character.bank_gold,
      gold: character.gold,
      bank_rank: 0,
      bank_tax: 0
    }
  end
end
