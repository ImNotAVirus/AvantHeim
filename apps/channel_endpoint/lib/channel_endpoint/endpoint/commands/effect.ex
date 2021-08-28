defmodule ChannelEndpoint.Endpoint.EffectCommand do
  @moduledoc """
  TODO: Documentation
  """

  alias Core.Socket
  alias ChannelEndpoint.Endpoint.ChatViews
  alias ChannelEndpoint.Endpoint.EntityInteractions

  ## Public API

  # > $effect
  # Usage: $effect show value:integer [on] [player_name:string]
  #
  # $effect show test
  # Invalid value 'test'
  # Usage: $effect show value:integer [on] [player_name:string]
  #
  # $effect show 5098
  # [show effect on yourself in game]
  #
  # $effect show 5098 on UnknownUser
  # No logged user found with name: UnknownUser
  #
  # $effect show 5098 on DarkyZ
  # [show effect on DarkyZ in game]
  def handle_command("$effect", args, socket) do
    %{character_id: character_id} = socket.assigns
    {:ok, character} = CachingService.get_character_by_id(character_id)

    case args do
      ["show", str_val] ->
        handle_effect(socket, args, str_val, character, '')

      ["show", str_val, "on", name] ->
        handle_effect(socket, args, str_val, character, name)

      args ->
        send_message(socket, character, usage(args), :special_red)
    end

    {:cont, socket}
  end

  ## Private functions

  defp usage(_), do: "Usage: $effect show value:integer"

  defp handle_effect(socket, args, str_val, character, name) do
    {:ok, new_char} = CachingService.get_character_by_name(name)

    case Integer.parse(str_val) do
      {value, ""} ->
        EntityInteractions.show_effect(new_char || character, value)

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
