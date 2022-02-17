defmodule TypeInspector do
  def __after_compile__(_env, bytecode) do
    {:ok, types} = Code.Typespec.fetch_types(bytecode)

    type_to_quoted = fn {_king, type} ->
      type
      |> Code.Typespec.type_to_quoted()
      |> Macro.to_string()
      |> IO.puts()
    end

    Enum.each(types, type_to_quoted)
  end
end
