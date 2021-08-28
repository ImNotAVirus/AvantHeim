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
  # Usage: $gold <get|set|add|sub> [from] [player_name:string] | [value:integer:0-2_000_000_000] [to] [player_name:string]
  #
  # > $gold set test
  # Invalid value 'test'
  # Usage: $gold <set> [value:integer:0-2_000_000_000] [to] [player_name:string]
  #
  # > $gold set 2_000_000_001
  # DarkyZ has now 2_000_000_000 golds
  #
  # > $gold set 666
  # DarkyZ has now 666 golds
  #
  # > $gold add 3
  # DarkyZ has now 669 golds
  #
  # > $gold sub 3
  # DarkyZ has now 666 golds
  #
  # > $gold set -1
  # DarkyZ has now 0 golds
  #
  # > $gold set 0 to UnknowPlayer
  # UnknowPlayer is not logged
  #
  # > $gold set 3 to Fizo
  # Fizo has now 3 golds
  #
  # > $gold get
  # Current DarkyZ's gold: 3 golds
  #
  # > $gold get from Fizo
  # > Current Fizo's gold: 3 golds
  #
  # > $gold add 3 to Fizo
  # > Fizo has now 6 golds
  #
  # > $gold sub 6 to Fizo
  # > Fizo has now 0 golds

  @op_types ["set", "add", "sub"]

  @spec handle_command(String.t(), [String.t()], Socket.t()) :: {:cont, Socket.t()}
  def handle_command("$gold", args, socket) do
    %{character_id: character_id} = socket.assigns
    {:ok, character} = CachingService.get_character_by_id(character_id)

    case args do
      ["get"] ->
        get_golds(socket, character, args, character)

      ["get", "from", name] ->
        apply_on_character(socket, character, args, name, &get_golds/4)

      [op_type, _] = args when op_type in @op_types ->
        update_golds(socket, character, args, character)

      [op_type, _, "to", name] = args when op_type in @op_types ->
        apply_on_character(socket, character, args, name, &update_golds/4)

      args ->
        send_message(socket, character, usage(args), :special_red)
    end

    {:cont, socket}
  end

  ## Private functions

  defp usage(["get" | _]), do: "Usage: $gold get [from] [player_name:string]"

  defp usage(["set" | _]),
    do: "Usage: $gold set [value:integer:0-2_000_000_000] [to] [player_name:string]"

  defp usage(["add" | _]),
    do: "Usage: $gold add [value:integer:0-2_000_000_000] [to] [player_name:string]"

  defp usage(["sub" | _]),
    do: "Usage: $gold sub [value:integer:0-2_000_000_000] [to] [player_name:string]"

  defp usage(_) do
    "Usage: $gold <get|set|add|sub> [from] [player_name:string] | " <>
      "[value:integer:0-2_000_000_000] [to] [player_name:string]"
  end

  @typep callback :: (Socket.t(), Character.t(), [String.t()], Character.t() -> :ok)
  @spec apply_on_character(Socket.t(), Character.t(), [String.t()], String.t(), callback) :: :ok
  defp apply_on_character(socket, character, args, name, callback) do
    case CachingService.get_character_by_name(name) do
      {:ok, nil} ->
        send_message(socket, character, "#{name} is not logged", :special_red)

      {:ok, target} ->
        callback.(socket, character, args, target)
    end
  end

  @spec get_golds(Socket.t(), Character.t(), [String.t()], Character.t()) :: :ok
  defp get_golds(socket, character, [_, str_val | _] = args, target) do
    send_message(
      socket,
      character,
      "Current #{target.name}'s gold: #{target.gold} golds",
      :special_green
    )
  end

  @spec update_golds(Socket.t(), Character.t(), [String.t()], Character.t()) :: :ok
  defp update_golds(socket, character, [op_type, str_val | _] = args, target) do
    case Integer.parse(str_val) do
      {value, ""} ->
        updated_gold = compute_golds(op_type, target.gold, value)
        {:ok, new_char} = EntityInteractions.set_player_golds(target, updated_gold)

        send_message(
          socket,
          new_char,
          "#{new_char.name} has now #{new_char.gold} golds",
          :special_green
        )

      _ ->
        send_message(socket, character, "Invalid value '#{str_val}'", :special_red)
        send_message(socket, character, usage(args), :special_red)
    end
  end

  defp compute_golds("set", golds, value), do: value
  defp compute_golds("add", golds, value), do: golds + value
  defp compute_golds("sub", golds, value), do: golds - value

  defp send_message(socket, character, msg, color) do
    render = ChatViews.render(:say, %{entity: character, color: color, message: msg})
    Socket.send(socket, render)
  end
end
