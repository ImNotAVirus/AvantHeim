defmodule ElvenViews.SerializablePacket do
  @moduledoc """
  TODO: Documentation.
  """

  @native_types [:integer, :pos_integer, :non_neg_integer, :boolean, :list]
  @type_aliases [string: String, enum: ElvenViews.SerializableEnum]
  @aliased_types Keyword.keys(@type_aliases)
  @supported_types @native_types ++ @aliased_types

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
      unquote(define_type(env))
      unquote(def_interface())
      unquote(def_serialize(env))
    end
  end

  @doc """
  TODO: Documentation
  """
  defmacro defpacket(name, do: exp) do
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
    updated_opts = Keyword.delete(opts, :default)

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
        default: unquote(escaped_default)
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
      Enum.map(@fields, &{&1.name, &1.default})
    end
  end

  defp define_type(env) do
    quote do
      @type t :: unquote(ast_type(env))
    end
  end

  defp ast_type(env) do
    ast_type_fun = fn
      field when field.type in @aliased_types -> {field.name, ast_t(@type_aliases[field.type])}
      field when field.type in @supported_types -> {field.name, field.type}
      field -> {field.name, ast_t(field.type)}
    end

    fields_ast =
      env.module
      |> Module.get_attribute(:fields)
      |> Enum.map(ast_type_fun)

    # %__MODULE__{fields_ast}
    {:%, [], [env.module, {:%{}, [], fields_ast}]}
  end

  defp ast_t(mod) do
    # mod.t()
    {{:., [], [mod, :t]}, [], []}
  end

  defp def_interface() do
    quote do
      def new!(attrs) do
        attrs
        |> unquote(__MODULE__).prepare_attrs(@fields)
        |> then(&struct!(__MODULE__, &1))
      end
    end
  end

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

  defp serialize_field_ast(%{type: _, name: name, opts: opts, default: default}) do
    value_ast = maybe_default_ast(name, default)

    case opts do
      [] ->
        value_ast

      _ ->
        quote do
          serialize_term(unquote(value_ast), unquote(opts))
        end
    end
  end

  defp maybe_default_ast(name, default) do
    case default do
      nil ->
        quote do
          struct.unquote(name)
        end

      _ ->
        quote do
          struct.unquote(name) || unquote(default)
        end
    end
  end

  ## Internal functions

  @doc false
  def prepare_attrs(attrs, fields) do
    attrs
    |> extract_args!(fields)
    |> Enum.into(%{})
  end

  ## Private helpers

  defp resolve_type!(type) when type in @supported_types, do: type

  defp resolve_type!(type) do
    # Is type a module? 
    Code.ensure_loaded?(type) || raise "invalid type #{inspect(type)}"

    # Is module implement the `SerializerProtocol` protocol
    protocol = ElvenCore.Socket.SerializerProtocol
    protocol_mod = Module.concat(protocol, type)

    Code.ensure_loaded?(protocol_mod) ||
      raise "#{inspect(protocol)} not implemented for #{inspect(type)}"

    # Ok :)
    type
  end

  defp extract_args!(attrs, fields) do
    Enum.map(fields, fn field ->
      error = "no value provided for required field #{inspect(field.name)}"

      case {attrs[field.name], field.default} do
        {nil, nil} -> raise ArgumentError, error
        {nil, default} -> {field.name, default}
        {value, _} -> {field.name, value}
      end
    end)
  end
end
