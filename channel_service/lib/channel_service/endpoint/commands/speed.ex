defmodule ChannelService.Endpoint.SpeedCommand do
  @moduledoc """
  TODO: Documentation
  """

  alias ElvenCore.Socket
  alias ChannelService.Endpoint.ChatViews
  alias ChannelService.Endpoint.EntityInteractions

  ## Public API

  # > $speed
  # Usage: $speed <get|set> [value:integer]
  #
  # > $speed set test
  # Invalid value 'test'
  # Usage: $speed <get|set> [value:integer]
  # 
  # > $speed set 50
  # Your speed is now 50
  #
  # > $speed get
  # Current speed: 50
  @spec handle_command(String.t(), [String.t()], Socket.t()) :: {:cont, Socket.t()}
  def handle_command("$speed", args, socket) do
    %{character_id: character_id} = socket.assigns
    {:ok, character} = CachingService.get_character_by_id(character_id)

    case args do
      ["get"] ->
        send_message(socket, character, "Current speed: #{character.speed}", :special_green)

      ["set", str_val] = args ->
        case Integer.parse(str_val) do
          {value, ""} when value in 0..59 ->
            {:ok, new_char} = EntityInteractions.set_speed(character, value)
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
