defmodule ChannelEndpoint.Endpoint.SpeedCommand do
  @moduledoc """
  TODO: Documentation
  """

  alias Core.Socket
  alias CachingService.Player.Character
  alias ChannelEndpoint.Endpoint.{ChatViews, EntityViews}

  ## Public API

  # > $speed
  # Usage: $speed <get|set> [value:integer]
  #
  # > $speed test
  # Unknown args 'test' for `$speed`
  # Usage: $speed <get|set> [value:integer]
  #
  # > $speed set test
  # Invalid value 'test' for `$speed`
  # Usage: $speed <get|set> [value:integer]
  #
  # > $speed get
  # Current speed: 30
  # 
  # > $speed set 50
  # Your speed is now 50

  def handle_command("$speed", args, socket) do
    %{character_id: character_id} = socket.assigns
    {:ok, character} = CachingService.get_character_by_id(character_id)

    case args do
      [] ->
        send_message(socket, character, usage(nil), :special_red)

      ["get"] ->
        send_message(socket, character, "Current speed: #{character.speed}", :special_green)

      ["set", str_val] ->
        case Integer.parse(str_val) do
          {value, ""} ->
            {:ok, new_char} = CachingService.write_character(%Character{character | speed: value})
            Socket.send(socket, EntityViews.render(:cond, new_char))
            send_message(socket, character, "Your speed is now #{value}", :special_green)

          _ ->
            send_message(
              socket,
              character,
              "Invalid value '#{str_val}' for `$speed`",
              :special_red
            )

            send_message(socket, character, usage("set"), :special_red)
        end

      [sub_command | _] ->
        sub_command
        |> usage()
        |> then(&send_message(socket, character, &1, :special_red))
    end

    {:cont, socket}
  end

  ## Private functions

  defp usage("get"), do: "Usage: $speed get"
  defp usage("set"), do: "Usage: $speed set value:integer"
  defp usage(_), do: "Usage: $speed <get|set> [value:integer]"

  defp send_message(socket, character, msg, color) do
    render = ChatViews.render(:say, %{entity: character, color: color, message: msg})
    Socket.send(socket, render)
  end
end
