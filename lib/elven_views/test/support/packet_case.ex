defmodule PacketCase do
  use ExUnit.CaseTemplate

  setup do
    # This will run before each test that uses this case
    :ok
  end

  using do
    quote do
      import ElvenCore.Socket.Serializer, only: [serialize_term: 1, serialize_term: 2]

      defp structure_to_iolist(struct, opts \\ []) do
        struct
        |> struct.__struct__.serialize(opts)
        |> Enum.reject(&(&1 == :"$drop"))
        |> Enum.map(&serialize_term(&1, opts))
      end

      # WTF is dat function ??? xDDD
      def make_custom_env(mod) do
        __ENV__
        |> Map.update!(:requires, &[mod | &1])
        |> Map.update!(:functions, &[{mod, mod.__info__(:functions)} | &1])
      end

      defp structure_default(struct, key) do
        struct_mod = struct.__struct__
        default = struct_mod.__fields__(key).default
        {eval, _} = Code.eval_quoted(default, [], make_custom_env(struct_mod))
        eval || raise "do default key found for #{inspect(struct_mod)}.#{key}"
      end

      defp structure_enum_default(struct, enum_name) do
        struct_mod = struct.__struct__
        kv = struct_mod.__fields__(enum_name).opts[:values]
        key = structure_default(struct, enum_name)
        Keyword.fetch!(kv, key)
      end

      defp structure_enum_value(struct, enum_name) do
        struct_mod = struct.__struct__
        kv = struct_mod.__fields__(enum_name).opts[:values]
        key = Map.fetch!(struct, enum_name)
        Keyword.fetch!(kv, key)
      end

      defp packet_index(packet, index) do
        Enum.at(packet, index)
      end
    end
  end
end
