defmodule ChannelEndpoint.Endpoint.EffectCommand do
  @moduledoc """
  TODO: Documentation
  """

  alias Core.Socket
  alias ChannelEndpoint.Endpoint.ChatViews
  alias ChannelEndpoint.Endpoint.EntityInteractions

  ## Public API

  # > $effect
  # Usage: $effect show value:integer
  #
  # > $effect test
  # Invalid value 'test'
  #
  # $effect show 5098
  # Show effect in game
  def handle_command("$effect", args, socket) do
    %{character_id: character_id} = socket.assigns
    {:ok, character} = CachingService.get_character_by_id(character_id)

    case args do
      [] = args ->
        send_message(socket, character, usage(args), :special_red)

      ["show", str_val] ->
        case Integer.parse(str_val) do
          {value, ""} ->
            EntityInteractions.show_effect(character, value)
        end

      args ->
        send_message(socket, character, usage(args), :special_red)
    end

    {:cont, socket}
  end

  ## Private functions

  defp usage(_), do: "Usage: $effect show value:integer"

  defp send_message(socket, character, msg, color) do
    render = ChatViews.render(:say, %{entity: character, color: color, message: msg})
    Socket.send(socket, render)
  end
end
