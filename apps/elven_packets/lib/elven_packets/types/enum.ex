defmodule ElvenPackets.Types.NsEnum do
  @moduledoc """
  A specific value from a given list

  ===

  The list of possible values and how each is encoded as an X must be known
  from the context
  """

  use ElvenGard.Network.Type

  alias ElvenPackets.Types.NsInteger

  @type t :: :atom

  ## Behaviour impls

  @impl true
  @spec decode(binary(), Keyword.t()) :: {t(), binary()}
  def decode(data, opts) when is_binary(data) do
    enumerators = Keyword.fetch!(opts, :values)

    {value, rest} = NsInteger.decode(data)
    {key, _v} = Enum.find(enumerators, &match?({_, ^value}, &1))

    {key, rest}
  end

  @impl true
  @spec encode(t(), Keyword.t()) :: binary()
  def encode(key, opts) when is_atom(key) do
    enumerators = Keyword.fetch!(opts, :values)
    apply = Keyword.get(opts, :apply)

    case Enum.find(enumerators, &match?({^key, _}, &1)) do
      {_k, value} -> maybe_apply(value, apply)
      nil -> raise ArgumentError, "invalid key #{inspect(key)} in #{inspect(enumerators)}"
    end
  end

  ## Helpers

  defp maybe_apply(value, nil) do
    NsInteger.encode(value)
  end

  defp maybe_apply(value, apply) do
    apply.(value)
  end
end
