defmodule ChannelEndpoint.Endpoint.MapActions do
  @moduledoc """
  TODO: Documentation
  """

  alias Core.Socket
  alias ChannelEndpoint.Endpoint.EntityInteractions

  ## Packet handlers

  @spec walk(String.t(), map, Socket.t()) :: {:cont, Socket.t()}
  def walk("walk", params, %Socket{} = socket) do
    %{
      pos_x: pos_x,
      pos_y: pos_y,
      speed: speed
    } = params

    %{character_id: character_id} = socket.assigns
    {:ok, character} = CachingService.get_character_by_id(character_id)

    # TODO: Later do some security checks (distance, checksum, dest cell, etc...)
    if speed != character.speed do
      raise "[walk] Invalid speed detected for #{character.name} (#{character_id})"
    end

    {:ok, _} = EntityInteractions.move(character, pos_x, pos_y)

    {:cont, socket}
  end

  @spec sit(String.t(), map, Socket.t()) :: {:cont, Socket.t()}
  def sit("rest", params, %Socket{} = socket) do
    IO.inspect(params)
    {:cont, socket}
  end
end
