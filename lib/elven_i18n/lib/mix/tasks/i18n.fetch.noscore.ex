defmodule Mix.Tasks.I18n.Fetch.Noscore do
  use Mix.Task

  @url "https://raw.githubusercontent.com/NosCoreIO/NosCore.Packets/master/src/NosCore.Packets/Enumerations/Game18NConstString.cs"
  @target_folder :code.priv_dir(:elven_i18n)
  @filename "Game18NConstString.yaml"
  @match_type ~r/\(([[:alpha:]]+)\)/

  @shortdoc "Pull Game18NConstString from NosCore.Packets"
  def run(_opts) do
    :inets.start()
    :ssl.start()

    url = String.to_charlist(@url)
    path = @target_folder |> Path.join(@filename) |> String.to_charlist()
    opts = [ssl: [verify: :verify_none]]

    {:ok, {{_, 200, _}, _, result}} = :httpc.request(:get, {url, []}, opts, [])

    yaml =
      result
      |> to_string()
      |> String.split("{", parts: 3)
      |> Enum.at(2)
      |> String.split(",\n")
      |> Enum.map(&parse_const_string/1)
      |> to_yaml()

    File.write!(path, yaml)
  end

  ## Private functions

  defp parse_const_string(block) do
    default_state = %{key: nil, value: nil, text: nil, args: []}

    block
    |> String.split("\n", trim: true)
    |> Enum.map(&String.trim/1)
    |> Enum.reduce(default_state, &parse_line/2)
  end

  defp parse_line("}", state), do: state
  defp parse_line("// <summary>", state), do: state
  defp parse_line("// " <> text, state) when is_nil(state.text), do: %{state | text: text}

  defp parse_line("[Game18NArguments(" <> arguments, state) when state.args == [] do
    args =
      @match_type
      |> Regex.scan(arguments, capture: :all_but_first)
      |> Enum.map(fn
        ["string"] -> :string
        ["long"] -> :integer
        type -> raise "unknown type #{inspect(type)}"
      end)

    %{state | args: args}
  end

  defp parse_line(kv, state) when is_nil(state.key) and is_nil(state.value) do
    [key, value] = String.split(kv, " = ")
    %{state | key: key, value: String.to_integer(value)}
  end

  defp to_yaml(states) do
    do_to_yaml = fn state ->
      """
      ---
      # #{state.text}
      key: #{state.key}
      value: #{state.value}
      args: [#{Enum.join(state.args, ",")}]
      """
    end

    states
    |> Enum.filter(&(not is_nil(&1.key)))
    |> Enum.map(do_to_yaml)
    |> Enum.join("\n")
  end
end
