defmodule ElvenViews.SubPackets.I18nSubPacket do
  @moduledoc false

  # TODO: Find a way to have named params
  # eg. for key BalanceBank: [bank_gold, gold]

  use ElvenCore.SerializableStruct

  alias __MODULE__
  alias ElvenI18n.PacketConstString

  @enforce_keys [:key]
  @additional_keys [args: []]
  defstruct @enforce_keys ++ @additional_keys

  @type t :: %I18nSubPacket{key: String.t(), args: list()}

  ## SerializableStruct behaviour

  @impl true
  def serialize(%I18nSubPacket{key: key, args: args}, _) do
    i18n_data = PacketConstString.new!(key, args)

    case length(args) do
      0 -> iolist_for_empty_args(i18n_data)
      _ -> iolist_for_non_empty_args(i18n_data)
    end
    |> serialize_term(joiner: " ")
  end

  ## Private functions

  defp iolist_for_empty_args(%{value: value}) do
    [value, 0]
  end

  defp iolist_for_non_empty_args(%{value: value, args: args}) do
    List.flatten([value, length(args) + 1, args, 0])
  end
end
