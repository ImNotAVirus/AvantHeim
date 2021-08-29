defmodule ChannelEndpoint.Endpoint.UIViews do
  @moduledoc """
  TODO: Documentation
  """

  alias CachingService.Player.Character
  alias ChannelEndpoint.Endpoint.UIPackets.{Cancel, Info, Scene, Gold, Gb, SMemoi2, SMemoi}

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
  def render(:gb, %{
        entity: %Character{gold: gold, bank_gold: bank_gold},
        bank_action_type: bank_action_type,
        bank_rank: bank_rank,
        bank_tax: bank_tax
      }) do
    %Gb{
      bank_action_type: bank_action_type,
      gold_bank: bank_gold,
      gold: gold,
      bank_rank: bank_rank,
      bank_tax: bank_tax
    }
  end

  def render(:smemoi2, %{
        entity: %Character{gold: gold, bank_gold: bank_gold},
        text_type: text_type,
        i18n_string: i18n_string
      }) do
    %SMemoi2{
      text_type: text_type,
      i18n_string: i18n_string,
      gold_bank: bank_gold,
      gold: gold
    }
  end

  def render(:smemoi, %{text_type: text_type, i18n_string: i18n_string}) do
    %SMemoi{
      text_type: text_type,
      i18n_string: i18n_string
    }
  end
end
