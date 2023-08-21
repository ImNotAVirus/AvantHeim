defmodule ElvenPackets.SubPackets.I18nSubPacket do
  @moduledoc false

  # TODO: Find a way to have named params
  # eg. for key BalanceBank: [bank_gold, gold]

  use ElvenGard.Network.Type

  alias __MODULE__
  alias ElvenI18n.PacketConstString
  alias ElvenPackets.Types.{NsString, NsList, NsInteger}

  @enforce_keys [:key]
  @additional_keys [args: []]
  defstruct @enforce_keys ++ @additional_keys

  @type t :: %I18nSubPacket{key: String.t(), args: list()}

  ## SerializableStruct behaviour

  @impl true
  @spec encode(t(), Keyword.t()) :: binary()
  def encode(%I18nSubPacket{key: key, args: args}, _) do
    i18n_data = PacketConstString.new!(key, args)

    case length(args) do
      0 -> iolist_for_empty_args(i18n_data)
      _ -> iolist_for_non_empty_args(i18n_data)
    end
    |> Enum.map(fn
      value when is_binary(value) -> value
      value when is_integer(value) -> NsInteger.encode(value)
    end)
    |> NsList.encode(joiner: " ", type: NsString)
  end

  @impl true
  def decode(_data, _opts) do
    # We do this to trick Dialyzer to not complain about non-local returns.
    case :erlang.phash2(1, 1) do
      0 -> raise("unimplemented decoder for #{inspect(__MODULE__)}")
      1 -> {nil, ""}
    end
  end

  ## Private functions

  defp iolist_for_empty_args(%{value: value}) do
    [value, 0]
  end

  defp iolist_for_non_empty_args(%{value: value, args: args}) do
    List.flatten([value, length(args) + 1, args, 0])
  end
end
