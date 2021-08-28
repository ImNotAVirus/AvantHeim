defmodule ChannelEndpoint.Endpoint.GoldCommand do
  @moduledoc """
  TODO: Documentation.
  """

  alias Core.Socket
  alias ChannelEndpoint.Endpoint.ChatViews
  alias ChannelEndpoint.Endpoint.EntityInteractions
  alias CachingService.Player.Character

  ## Public API

  # > $gold
  # Usage: $gold <get|set> [value:integer]
  #
  # > $gold set test
  # Invalid value 'test'
  #
  # > $gold set 2_000_000_001
  # DarkyZ have now 2_000_000_000 golds
  #
  # > $gold set 666
  # DarkyZ have now 666 golds
  #
  # > $gold set -1
  # DarkyZ have now 0 golds
  #
  # > $gold set 0 to UnknowPlayer
  # UnknowPlayer is not logged
  #
  # > $gold set 3 to DarkyZ
  # DarkyZ have now 3 golds
  #
  # > $gold get
  # Current DarkyZ gold: 0 golds
  #
  # > $gold get from DarkyZ
  # > Current DarkyZ gold: #{target.gold} golds

  @spec handle_command(String.t(), [String.t()], Socket.t()) :: {:cont, Socket.t()}
  def handle_command("$gold", args, socket) do
    %{character_id: character_id} = socket.assigns
    {:ok, character} = CachingService.get_character_by_id(character_id)

    case args do
      ["get"] ->
        get_gold(socket, character, args, character)

      ["get", "from", name] ->
        apply_on_character(socket, character, args, name, &get_gold/4)

      ["set", _] = args ->
        set_gold(socket, character, args, character)

      ["set", _, "to", name] = args ->
        apply_on_character(socket, character, args, name, &set_gold/4)

      args ->
        send_message(socket, character, usage(args), :special_red)
    end

    {:cont, socket}
  end

  ## Private functions

  defp usage(["get" | _]), do: "Usage: $gold <get>"
  defp usage(["set" | _]), do: "Usage: $gold <set> [value:integer:0-2_000_000_000] [to] [player_name:string]"
  defp usage(_), do: "Usage: $gold <get|set> [value:integer:0-2_000_000_000] [to] [player_name:string] [from] [player_name:string]"

  @spec apply_on_character(Socket.t(), Character.t(), [String.t()], String.t(), any) :: {:ok, Character.t()}
  defp apply_on_character(socket, character, args, name, callback) do
    case CachingService.get_character_by_name(name) do
      {:ok, nil} ->
        send_message(socket, character, "#{name} is not logged", :special_red)

      {:ok, target} ->
        callback.(socket, character, args, target)
    end
  end

  defp get_gold(socket, character, [_, str_val | _] = args, target) do
    send_message(socket, character, "Current #{target.name} gold: #{target.gold} golds", :special_green)
  end

  @spec set_gold(Socket.t(), Character.t(), [String.t()], Character.t()) :: :ok
  defp set_gold(socket, character, [_, str_val | _] = args, target) do
    case Integer.parse(str_val) do
      {value, ""} ->
        {:ok, new_char} = EntityInteractions.set_player_gold(target, value)
        send_message(socket, new_char, "#{new_char.name} have now #{new_char.gold} golds", :special_green)

      _ ->
        send_message(socket, character, "Invalid value '#{str_val}'", :special_red)
        send_message(socket, character, usage(args), :special_red)
    end
  end

  defp send_message(socket, character, msg, color) do
    render = ChatViews.render(:say, %{entity: character, color: color, message: msg})
    Socket.send(socket, render)
  end
end
