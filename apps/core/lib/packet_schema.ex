defmodule Core.PacketSchema do
  @moduledoc """
  TODO: Documentation
  """

  ## Public API

  @doc false
  defmacro __using__(_) do
    quote do
      import unquote(__MODULE__), only: :macros

      @before_compile unquote(__MODULE__)

      unquote(prelude())
    end
  end

  @doc """
  Define a new packet handler
  """
  defmacro packet(header, do: exp) do
    quote do
      unquote(init_definition(header))
      unquote(exp)
      unquote(persist_definition())
      unquote(reset_context())
    end
  end

  defmacro field(name, type, opts \\ []) do
    quote do
      @packet_fields {unquote(name), unquote(type), unquote(Keyword.get(opts, :using))}
    end
  end

  defmacro resolve(module, function) do
    expanded_module = Macro.expand(module, __CALLER__)

    quote do
      @packet_resolver {unquote(expanded_module), unquote(function)}
    end
  end

  ## Internal macros

  @doc false
  defmacro __before_compile__(env) do
    env.module
    |> Module.get_attribute(:packet_defs)
    |> Enum.reverse()
    |> Enum.map(fn def ->
      %{
        doc: doc,
        header: header,
        fields: fields,
        resolver: resolver
      } = def

      field_names = Enum.map(fields, &elem(&1, 0))
      field_types = Enum.map(fields, &elem(&1, 1))
      args_ast = fields_to_args_ast(header, fields)
      {resolver_module, resolver_function} = resolver

      quote do
        defdelegate unquote(:"resolve_#{header}")(header, args, socket),
          to: unquote(resolver_module),
          as: unquote(resolver_function)

        @doc unquote(doc)
        def unquote(:"parse_#{header}!")(unquote(args_ast) = split, socket) do
          [header | str_args] = split

          [str_args, unquote(field_names), unquote(field_types)]
          |> Enum.zip()
          |> Enum.map(&unquote(__MODULE__).parse_arg!/1)
          |> Enum.into(%{})
        end
      end
    end)
  end

  ## Internal functions

  @doc false
  def parse_arg!({value, name, :string}), do: {name, value}
  def parse_arg!({value, name, :integer}), do: {name, String.to_integer(value)}
  def parse_arg!({_, name, type}), do: raise("unknown field type: #{type} for #{name}")

  ## Private functions

  defp prelude() do
    quote do
      Module.register_attribute(__MODULE__, :packet_defs, accumulate: true)
      unquote(reset_context())
    end
  end

  defp init_definition(header) do
    quote do
      @packet_resolver nil
      @packet_header unquote(header)
      Module.register_attribute(__MODULE__, :packet_fields, accumulate: true)
    end
  end

  defp persist_definition() do
    quote do
      if is_nil(@packet_resolver) do
        raise "packet resolver must be defined for #{inspect(__MODULE__)}:#{@packet_header}"
      end

      doc =
        case Module.get_attribute(__MODULE__, :doc) do
          nil -> nil
          {_, doc} -> doc
        end

      @packet_defs %{
        doc: doc,
        header: @packet_header,
        fields: Enum.reverse(@packet_fields),
        resolver: @packet_resolver
      }
    end
  end

  defp reset_context() do
    quote do
      Module.delete_attribute(__MODULE__, :doc)
      Module.delete_attribute(__MODULE__, :packet_header)
      Module.delete_attribute(__MODULE__, :packet_fields)
      Module.delete_attribute(__MODULE__, :packet_resolver)
    end
  end

  defp fields_to_args_ast(header, fields) do
    fields
    |> Enum.map(fn
      {name, _type, nil} -> {name, [], __MODULE__}
      {name, _type, using} -> {:=, [], [using, {name, [], __MODULE__}]}
    end)
    |> List.insert_at(0, header)
  end
end
