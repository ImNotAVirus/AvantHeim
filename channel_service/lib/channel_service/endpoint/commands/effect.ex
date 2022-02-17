defmodule ChannelService.Endpoint.EffectCommand do
  @moduledoc """
  TODO: Documentation
  """

  alias ElvenCore.Socket
  alias ChannelService.Endpoint.ChatViews
  alias ChannelService.Endpoint.EntityInteractions
  alias ElvenCaching.CharacterRegistry

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
    {:ok, character} = CharacterRegistry.get(character_id)

    case args do
      ["show", _] = args ->
        handle_effect(socket, character, args, character)

      ["show", _, "on", name] = args ->
        case CharacterRegistry.get_by_name(name) do
          {:ok, target} ->
            handle_effect(socket, character, args, target)

          {:error, :not_found} ->
            send_message(socket, character, "#{name} is not logged", :special_red)
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
