defmodule ChannelEndpoint.Endpoint.GoldCommand do
  @moduledoc """
  TODO: Documentation.
  """

  alias Core.Socket
  alias ChannelEndpoint.Endpoint.ChatViews
  alias ChannelEndpoint.Endpoint.EntityInteractions

  ## Public API

  # > $gold
  # Usage: $gold <get|set> [value:integer]
  #
  # > $gold set test
  # Invalid value 'test'
  #
  # > $gold set 2_000_000_001
  # Gold set to 2_000_000_000
  #
  # > $gold set 666
  # You have now 666 gold
  #
  # > $gold set -1
  # Gold set to 0
  #
  # > $gold get
  # Current gold: 0 golds

  @spec handle_command(String.t(), [String.t()], Socket.t()) :: {:cont, Socket.t()}
  def handle_command("$gold", args, socket) do
    %{character_id: character_id} = socket.assigns
    {:ok, character} = CachingService.get_character_by_id(character_id)

    case args do
      ["get"] ->
        send_message(socket, character, "Current gold: #{character.gold} golds", :special_green)

      ["set", str_val] = args ->
        case Integer.parse(str_val) do
          {value, ""} ->
            {:ok, new_char} = EntityInteractions.set_player_gold(character, value)
            send_message(socket, new_char, "You have now #{value} golds", :special_green)

          _ ->
            send_message(socket, character, "Invalid value '#{str_val}'", :special_red)
            send_message(socket, character, usage(args), :special_red)
        end

      args ->
        send_message(socket, character, usage(args), :special_red)
    end

    {:cont, socket}
  end

  ## Private functions

  defp usage(["get" | _]), do: "Usage: $gold get"
  defp usage(["set" | _]), do: "Usage: $gold set value:integer:0-2_000_000_000"
  defp usage(_), do: "Usage: $gold <get|set> [value:integer:0-2_000_000_000]"

  defp send_message(socket, character, msg, color) do
    render = ChatViews.render(:say, %{entity: character, color: color, message: msg})
    Socket.send(socket, render)
  end
end
