defmodule ChannelService.Endpoint.NameCommand do
  @moduledoc """
  TODO: Documentation
  """

  alias ElvenCaching.CharacterRegistry
  alias ElvenCaching.Entity.Character
  alias ElvenGard.Network.Socket
  alias ElvenPackets.Views.{ChatViews, UIViews}

  alias ChannelService.Endpoint.EntityInteractions

  @name_regex ~r/^[\x21-\x7E\xA1-\xAC\xAE-\xFF\x{4e00}-\x{9fa5}\x{0E01}-\x{0E3A}\x{0E3F}-\x{0E5B}\x2E]{4,14}$/u

  ## Public API

  # > $name
  # Usage: $name set value:string
  #
  # > $name set
  # Usage: $name set value:string
  #
  # > $name set [GM]Test
  # Invalid value '[GM]Test'
  # Usage: $name value:string
  #
  # > $name set Test
  # Your character name is now Test
  @spec handle_command(String.t(), [String.t()], Socket.t()) :: {:cont, Socket.t()}
  def handle_command("$name", ["set", name], socket) do
    %{character_id: character_id} = socket.assigns
    {:ok, character} = CharacterRegistry.get(character_id)

    if String.match?(name, @name_regex) do
      {:ok, new_char} = CharacterRegistry.write(%Character{character | name: name})

      Socket.send(socket, UIViews.render(:cancel, %{cancel_type: :action, entity: new_char}))
      EntityInteractions.send_map_enter(new_char)

      send_message(socket, new_char, "Your character name is now #{name}", :special_green)
    else
      send_message(socket, character, "Invalid value '#{name}'", :special_red)
      send_message(socket, character, usage(nil), :special_red)
    end

    {:cont, socket}
  end

  def handle_command("$name", _, socket) do
    %{character_id: character_id} = socket.assigns
    {:ok, character} = CharacterRegistry.get(character_id)
    send_message(socket, character, usage(nil), :special_red)
    {:cont, socket}
  end

  ## Private functions

  defp usage(_), do: "Usage: $name set value:string"

  defp send_message(socket, character, msg, color) do
    render = ChatViews.render(:say, %{entity: character, color: color, message: msg})
    Socket.send(socket, render)
  end
end
