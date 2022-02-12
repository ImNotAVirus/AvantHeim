defmodule DatabaseService.EctoEnumHelpers do
  @moduledoc """
  TODO: Documentation
  """

  @spec create_query(module, atom, Keyword.t()) :: any()
  defmacro create_query(module, enum, opts \\ []) do
    mod_ast =
      module
      |> Macro.expand(__CALLER__)
      |> Module.split()
      |> Enum.map(&String.to_atom/1)

    enum_name = Keyword.get(opts, :as, :"#{enum}_enum")

    quote location: :keep do
      enum_keys =
        unquote({{:., [], [{:__aliases__, [alias: false], mod_ast}, enum]}, [], [:__keys__]})
        |> Enum.map(&Atom.to_string/1)
        |> Enum.map(&"'#{&1}'")
        |> Enum.join(", ")

      "CREATE TYPE #{unquote(enum_name)} AS ENUM (#{enum_keys})"
    end
  end

  @spec drop_query(atom, Keyword.t()) :: any()
  defmacro drop_query(enum, opts \\ []) do
    enum_name = Keyword.get(opts, :as, :"#{enum}_enum")

    quote do
      "DROP TYPE #{unquote(enum_name)}"
    end
  end
end
