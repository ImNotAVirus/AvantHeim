defmodule ChannelService.Endpoint.EffectCommand do
  @moduledoc """
  TODO: Documentation
  """

  alias ElvenCore.Socket
  alias ChannelService.Endpoint.ChatViews
  alias ChannelService.Endpoint.EntityInteractions

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
  # UnknownUser is not logged
  #
  # $effect show 5098 on DarkyZ
  # [show effect on DarkyZ in game]
  @spec handle_command(String.t(), [String.t()], Socket.t()) :: {:cont, Socket.t()}
  def handle_command("$effect", args, socket) do
    %{character_id: character_id} = socket.assigns
    {:ok, character} = CachingService.get_character_by_id(character_id)

    case args do
      ["show", _] = args ->
        handle_effect(socket, character, args, character)

      ["show", _, "on", name] = args ->
        case CachingService.get_character_by_name(name) do
          {:ok, nil} ->
            send_message(socket, character, "#{name} is not logged", :special_red)

          {:ok, target} ->
            handle_effect(socket, character, args, target)
        end

      args ->
        send_message(socket, character, usage(args), :special_red)
    end

    {:cont, socket}
  end

  ## Private functions

  defp usage(_), do: "Usage: $effect show value:integer [on] [player_name:string]"

  defp handle_effect(socket, character, [_, str_val | _] = args, target) do
    case Integer.parse(str_val) do
      {value, ""} ->
        EntityInteractions.show_effect(target, value)

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
