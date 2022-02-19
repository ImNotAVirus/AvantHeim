defmodule ElvenViews.SerializablePacket do
  @moduledoc """
  TODO: Documentation.
  """

  @integer_types [:integer, :pos_integer, :non_neg_integer]
  @native_types @integer_types ++ [:boolean, :list]
  @type_aliases [string: String, enum: ElvenViews.SerializableEnum]
  @aliased_types Keyword.keys(@type_aliases)
  @supported_types @native_types ++ @aliased_types

  @type enum :: atom

  ## Public API

  @doc false
  defmacro __using__(_) do
    quote do
      @before_compile unquote(__MODULE__)

      use ElvenCore.SerializableStruct

      import unquote(__MODULE__), only: :macros
    end
  end

  @doc false
  defmacro __before_compile__(env) do
    quote do
      unquote(def_introspection())
      unquote(define_type(env))
      unquote(def_serialize(env))
    end
  end

  @doc """
  TODO: Documentation
  """
  defmacro defpacket(name, maybe_do \\ []) do
    exp = Keyword.get(maybe_do, :do, [])

    quote do
      if Module.get_attribute(__MODULE__, :name) do
        raise "can't define multiple packets is the same module: #{inspect(__MODULE__)}"
      end

      @name unquote(name)
      Module.register_attribute(__MODULE__, :fields, accumulate: true)

      unquote(exp)

      fields = @fields

      # Quick fix until I know if this behaviour is a bug or a feature
      # https://github.com/elixir-lang/elixir/issues/11635
      {set, _bag} = :elixir_module.data_tables(__MODULE__)
      :ets.delete(set, :fields)
      Module.delete_attribute(__MODULE__, :fields)

      Module.register_attribute(__MODULE__, :fields, accumulate: false)
      @fields Enum.reverse(fields) |> List.flatten()

      unquote(define_struct())
    end
  end

  @doc """
  TODO: Documentation
  """
  defmacro field(name, type, opts \\ []) do
    expanded_type = type |> Macro.expand(__CALLER__) |> resolve_type!()
    nullable = Keyword.get(opts, :nullable, false)

    updated_opts =
      opts
      |> Keyword.delete(:default)
      |> Keyword.delete(:nullable)

    escaped_default =
      case opts[:default] do
        {:-, _, _} = value -> value
        {_, _, _} = value -> Macro.escape(value)
        value -> value
      end

    quote do
      @fields %{
        name: unquote(name),
        type: unquote(expanded_type),
        opts: unquote(updated_opts),
        default: unquote(escaped_default),
        nullable: unquote(nullable)
      }
    end
  end

  ## AST fragments

  defp define_struct() do
    quote do
      @enforce_keys unquote(enforce_keys())
      defstruct unquote(struct_keys())
    end
  end

  defp enforce_keys() do
    quote do
      @fields
      |> Enum.reject(& &1.default)
      |> Enum.map(& &1.name)
    end
  end

  defp struct_keys() do
    quote do
      Enum.map(@fields, & &1.name)
    end
  end

  defp def_introspection() do
    quote unquote: false do
      def __header__(), do: @name
      def __fields__(), do: @fields

      Enum.each(@fields, fn field ->
        def __fields__(unquote(field.name)), do: unquote(Macro.escape(field))
      end)
    end
  end

  defp define_type(env) do
    quote do
      @type t :: unquote(ast_type(env))
    end
  end

  defp ast_type(env) do
    fields_ast =
      env.module
      |> Module.get_attribute(:fields)
      |> Enum.map(&{&1.name, ast_for_type(&1.type, &1.opts)})

    # %__MODULE__{fields_ast}
    {:%, [], [env.module, {:%{}, [], fields_ast}]}
  end

  defp ast_for_type(nil, _), do: raise("invalid nil type found")
  defp ast_for_type(:integer, _), do: quote(do: integer())
  defp ast_for_type(:pos_integer, _), do: quote(do: pos_integer())
  defp ast_for_type(:non_neg_integer, _), do: quote(do: non_neg_integer())
  defp ast_for_type(:boolean, _), do: quote(do: boolean())
  # TODO: use opts[:values]
  defp ast_for_type(:enum, _), do: quote(do: unquote(__MODULE__).enum())
  defp ast_for_type(:string, _), do: quote(do: String.t())
  defp ast_for_type(:list, opts), do: quote(do: list(unquote(ast_for_type(opts[:type], []))))
  defp ast_for_type(type, _), do: quote(do: unquote(type).t())

  defp def_serialize(env) do
    quote do
      @impl true
      def serialize(struct, _) do
        [@name | unquote(serialize_ast(env))]
      end
    end
  end

  defp serialize_ast(env) do
    fields = Module.get_attribute(env.module, :fields)
    Enum.map(fields, &serialize_field_ast/1)
  end

  defp serialize_field_ast(%{type: :enum, name: name, opts: opts, default: default}) do
    values = opts[:values]
    value_ast = maybe_default_ast(name, default)

    error =
      quote do
        "invalid key #{inspect(unquote(value_ast))} in enum #{inspect(__MODULE__)}.#{unquote(name)}"
      end

    quote do
      unquote(values)[unquote(value_ast)] || raise ArgumentError, unquote(error)
    end
  end

  defp serialize_field_ast(field) do
    %{
      type: type,
      name: name,
      opts: opts,
      default: default,
      nullable: nullable
    } = field

    value_ast = maybe_default_ast(name, default)

    # if opts: serialize_term(value_ast, opts)
    case {nullable, opts} do
      {true, _} -> nullable_type_ast(type, value_ast, opts)
      {false, []} -> value_ast
      {false, _} -> quote(do: serialize_term(unquote(value_ast), unquote(opts)))
    end
  end

  defp maybe_default_ast(name, default_ast) do
    value = quote(do: struct.unquote(name))

    # if default_ast: struct.name || default
    case default_ast do
      nil -> value
      _ -> {:||, [context: Elixir, import: Kernel], [value, default_ast]}
    end
  end

  defp nullable_type_ast(type, value_ast, opts) do
    new_opts =
      case type do
        :string -> Enum.concat([as: :string], opts)
        t when t in @integer_types -> Enum.concat([as: :integer], opts)
        _ -> raise "unsuported type #{inspect(type)} with nullable attribut"
      end

    quote do
      serialize_term(unquote(value_ast), unquote(new_opts))
    end
  end

  ## Private helpers

  defp resolve_type!(type) when type in @supported_types, do: type

  defp resolve_type!(type) do
    # Is type a module? 
    Code.ensure_compiled!(type)

    # Is module implement the `SerializerProtocol` protocol
    protocol = ElvenCore.Socket.SerializerProtocol
    protocol_mod = Module.concat(protocol, type)

    Code.ensure_loaded?(protocol_mod) ||
      raise "#{inspect(protocol)} not implemented for #{inspect(type)}"

    # Ok :)
    type
  end
end
