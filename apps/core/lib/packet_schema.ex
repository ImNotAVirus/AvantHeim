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
    |> Enum.map(&define_handler/1)
    |> Kernel.++([default_handlers()])
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

      @packet_defs %{
        header: @packet_header,
        fields: Enum.reverse(@packet_fields),
        resolver: @packet_resolver
      }
    end
  end

  defp reset_context() do
    quote do
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

  defp define_handler(def) do
    %{
      header: header,
      fields: fields,
      resolver: resolver
    } = def

    field_names = Enum.map(fields, &elem(&1, 0))
    field_types = Enum.map(fields, &elem(&1, 1))
    args_ast = fields_to_args_ast(header, fields)
    {resolver_module, resolver_function} = resolver

    quote do
      def resolve(unquote(header), args, socket) do
        apply(
          unquote(resolver_module),
          unquote(resolver_function),
          [unquote(header), args, socket]
        )
      end

      def parse_packet_args(unquote(args_ast) = split, socket) do
        [unquote(header) | str_args] = split

        args =
          [str_args, unquote(field_names), unquote(field_types)]
          |> Enum.zip()
          |> Enum.map(&unquote(__MODULE__).parse_arg!/1)
          |> Enum.into(%{})

        {:ok, {unquote(header), args}}
      end
    end
  end

  defp default_handlers() do
    quote do
      def resolve(header, args, socket) do
        raise "unkown resolver for #{header} with args #{inspect(args)} (from #{socket.id})"
      end

      def parse_packet_args(packet_args, socket) do
        {:error, "Invalid packet from #{socket.id}: #{inspect(packet_args)}"}
      end
    end
  end
end
