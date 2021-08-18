defmodule ChannelEndpoint.Endpoint.SpeedCommand do
  @moduledoc """
  TODO: Documentation
  """

  alias Core.Socket
  alias CachingService.Player.Character
  alias ChannelEndpoint.Endpoint.{ChatViews, EntityViews}
  alias ChannelEndpoint.Endpoint.PacketHelpers

  ## Public API

  # > $speed
  # Usage: $speed <get|set> [value:integer]
  #
  # > $speed test
  # Unknown args 'test'
  # Usage: $speed <get|set> [value:integer]
  #
  # > $speed set test
  # Invalid value 'test'
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
      [] = args ->
        send_message(socket, character, usage(args), :special_red)

      ["get"] ->
        send_message(socket, character, "Current speed: #{character.speed}", :special_green)

      ["set", str_val] = args ->
        case Integer.parse(str_val) do
          {value, ""} when value in 0..59 ->
            {:ok, new_char} = CachingService.write_character(%Character{character | speed: value})
            PacketHelpers.set_speed(new_char, socket)
            send_message(socket, new_char, "Your speed is now #{value}", :special_green)

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

  defp usage(["get" | _]), do: "Usage: $speed get"
  defp usage(["set" | _]), do: "Usage: $speed set value:integer:0-59"
  defp usage(_), do: "Usage: $speed <get|set> [value:integer:0-59]"

  defp send_message(socket, character, msg, color) do
    render = ChatViews.render(:say, %{entity: character, color: color, message: msg})
    Socket.send(socket, render)
  end
end
