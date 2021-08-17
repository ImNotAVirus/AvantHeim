defmodule ChannelEndpoint.PacketSchemas do
  @moduledoc """
  TODO: Documentation
  """

  use Core.PacketSchema

  alias ChannelEndpoint.Endpoint.{
    LobbyActions,
    PlayerActions
  }

  ## Ignore some packets

  ignore_packet "0"
  ignore_packet "c_close"
  ignore_packet "f_stash_end"
  ignore_packet "lbs"

  ## Lobby packets

  #######
  # Ask for a character creation
  # ---
  # Example: "Char_NEW TestChar 0 1 1 2"
  #######
  packet "Char_NEW" do
    field :name, :string
    field :slot, :integer
    field :gender, :integer, desc: "Enum: GenderType"
    field :hair_style, :integer, desc: "Enum: HairStyle"
    field :hair_color, :integer, desc: "Enum: HairColor"

    resolve LobbyActions, :create_character
  end

  #######
  # Ask for a character suppression
  # ---
  # Example: "Char_DEL 3 password"
  #######
  packet "Char_DEL" do
    field :slot, :integer
    field :password, :string

    resolve LobbyActions, :delete_character
  end

  #######
  # Select a character
  # ---
  # Example: "select 2"
  #######
  packet "select" do
    field :slot, :integer

    resolve LobbyActions, :select_character
  end

  #######
  # Enter in game
  # ---
  # Example: "game_start"
  #######
  packet "game_start" do
    resolve PlayerActions, :game_start
  end

  ## Commands
  ## TODO: Clean

  def parse_packet_args(["$speed" | args], _socket), do: {:ok, {"$speed", args}}
  def parse_packet_args(["$name" | args], _socket), do: {:ok, {"$name", args}}

  def resolve("$speed", args, socket) do
    %{character_id: character_id} = socket.assigns
    {:ok, character} = CachingService.get_character_by_id(character_id)

    :say
    |> ChannelEndpoint.Endpoint.ChatViews.render(%{
      entity: character,
      color: :special_gold,
      message: cmdline("$speed", args)
    })
    |> then(&Core.Socket.send(socket, &1))

    ChannelEndpoint.Endpoint.SpeedCommand.handle_command("$speed", args, socket)
  end

  def resolve("$name", args, socket) do
    %{character_id: character_id} = socket.assigns
    {:ok, character} = CachingService.get_character_by_id(character_id)

    :say
    |> ChannelEndpoint.Endpoint.ChatViews.render(%{
      entity: character,
      color: :special_gold,
      message: cmdline("$name", args)
    })
    |> then(&Core.Socket.send(socket, &1))

    ChannelEndpoint.Endpoint.NameCommand.handle_command("$name", args, socket)
  end

  @prefix ">"
  defp cmdline(cmd, args) do
    "#{@prefix} #{cmd} #{Enum.join(args, " ")}"
  end
end
