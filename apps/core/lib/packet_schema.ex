defmodule Core.PacketSchema do
  @moduledoc """
  TODO: Documentation
  """

  @aliases %{
    integer: Core.PacketSchema.Integer,
    string: Core.PacketSchema.String
  }

  ## Public API

  @doc false
  defmacro __using__(_) do
    quote do
      import unquote(__MODULE__), only: :macros

      @before_compile unquote(__MODULE__)

      unquote(prelude())
      unquote(define_helpers())
    end
  end

  @doc """
  Define an unused packet
  """
  defmacro ignore_packet(header) do
    quote do
      if Map.has_key?(@packet_defs, unquote(header)) do
        raise "can't ignore packet #{inspect(unquote(header))} because handler is already defined"
      end

      @packet_defs Map.put(@packet_defs, unquote(header), :ignore)
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
    expanded_type =
      type
      |> Macro.expand(__CALLER__)
      |> then(&Map.get(@aliases, &1, &1))
      |> Code.ensure_compiled!()

    quote do
      @packet_fields {unquote(name), unquote(expanded_type), unquote(opts)}
    end
  end

  defmacro resolve(module, function) do
    expanded_module =
      module
      |> Macro.expand(__CALLER__)
      |> Code.ensure_compiled!()

    quote do
      @packet_resolver {unquote(expanded_module), unquote(function)}
    end
  end

  ## Internal macros

  @doc false
  defmacro __before_compile__(_env) do
    quote do
      def parse({header, bin_args}, _socket, opts) do
        case Map.get(@packet_defs, header) do
          %{fields: fields} -> do_parse(header, bin_args, fields, opts)
          :ignore -> {:ignore, {header, bin_args}}
          nil -> {:error, :invalid, {header, bin_args}}
        end
      end

      def resolve(header, args, socket) do
        error = "unkown header #{inspect(header)} with args #{inspect(args)} (from #{socket.id})"

        case Map.get(@packet_defs, header) do
          %{resolver: {m, f}} -> apply(m, f, [header, args, socket])
          :ignore -> {:cont, socket}
          nil -> raise error
        end
      end
    end
  end

  ## Internal functions

  ## Private functions

  defp prelude() do
    quote do
      Module.register_attribute(__MODULE__, :packet_defs, persist: true)
      @packet_defs %{}

      unquote(reset_context())
    end
  end

  defp define_helpers() do
    quote do
      defp do_parse(header, bin_args, fields, opts) do
        case parse_fields(bin_args, fields, opts) do
          {:ok, fields} -> {:ok, {header, fields}}
          :error -> {:error, :invalid, {header, bin_args}}
        end
      end

      defp parse_fields(bin_args, field_defs, opts, results \\ %{})
      defp parse_fields("", [], _, results), do: {:ok, results}
      defp parse_fields("", _, _, results), do: :error
      defp parse_fields(_, [], _, results), do: :error

      defp parse_fields(bin_args, [{name, mod, field_opts} | rem_defs], parse_opts, results) do
        {using, rem_opts} = Keyword.pop(field_opts, :using)
        type_opts = Keyword.merge(parse_opts, rem_opts)
        parsed_val = apply(mod, :parse, [bin_args, type_opts])

        case {parsed_val, using} do
          {{:ok, value, rem_bin}, nil} ->
            parse_fields(rem_bin, rem_defs, parse_opts, Map.put(results, name, value))

          {{:ok, value, rem_bin}, u} when value == u ->
            parse_fields(rem_bin, rem_defs, parse_opts, Map.put(results, name, value))

          _ ->
            :error
        end
      end
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

      case Map.get(@packet_defs, @packet_header) do
        nil -> :ok
        :ignore -> raise "can't define packet #{inspect(@packet_header)} because it's ignored"
        _ -> raise "duplicate packet #{inspect(@packet_header)} found"
      end

      @packet_defs Map.put(@packet_defs, @packet_header, %{
                     fields: Enum.reverse(@packet_fields),
                     resolver: @packet_resolver
                   })
    end
  end

  defp reset_context() do
    quote do
      Module.delete_attribute(__MODULE__, :packet_header)
      Module.delete_attribute(__MODULE__, :packet_fields)
      Module.delete_attribute(__MODULE__, :packet_resolver)
    end
  end
end
