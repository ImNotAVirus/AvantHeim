defmodule ChannelEndpoint.Endpoint.EntityInteractions do
  @moduledoc """
  TODO: Break theses functions into another modules
  """

  alias Core.Socket
  alias CachingService.Position
  alias CachingService.Player.Character

  alias ChannelEndpoint.Endpoint.{
    EntityViews,
    MapViews,
    PlayerViews,
    VisibilityViews
  }

  # TODO : Improve that to support pnj | mobs | mates
  @spec show_effect(Character.t(), pos_integer) :: {:error, atom}
  def show_effect(%Character{} = character, effect_value) do
    broadcast_on_map(character, EntityViews.render(:eff, %{character: character, value: effect_value}))
  end

  @spec map_enter(Character.t()) :: :ok
  def map_enter(%Character{} = character) do
    ## Self packets
    Socket.send(character.socket, PlayerViews.render(:c_info, character))
    Socket.send(character.socket, EntityViews.render(:c_mode, character))
    Socket.send(character.socket, PlayerViews.render(:lev, character))
    Socket.send(character.socket, PlayerViews.render(:stat, character))
    Socket.send(character.socket, MapViews.render(:at, character))
    Socket.send(character.socket, MapViews.render(:c_map, character))
    # TODO: Socket.send(character.socket, PlayerViews.render(:sc, character))
    Socket.send(character.socket, EntityViews.render(:char_sc, character))
    Socket.send(character.socket, EntityViews.render(:cond, character))

    ## Other players packets
    %Position{map_id: map_id} = Character.get_position(character)

    {:ok, players} = CachingService.get_characters_by_map_id(map_id, [{:!==, :id, character.id}])
    Enum.each(players, &send_visibility_packets(character, &1))
  end

  @spec set_speed(Character.t(), 0..59) :: {:ok, new_char :: Character.t()} | {:error, atom}
  def set_speed(%Character{} = character, new_speed) do
    new_char = %Character{character | speed: new_speed}

    case CachingService.write_character(new_char) do
      {:ok, new_char} ->
        broadcast_on_map(new_char, EntityViews.render(:cond, new_char))
        {:ok, new_char}

      {:error, _} = x ->
        x
    end
  end

  @spec move(Character.t(), non_neg_integer, non_neg_integer) ::
          {:ok, new_char :: Character.t()} | {:error, atom}
  def move(%Character{} = character, new_x, new_y) do
    new_char = %Character{character | map_x: new_x, map_y: new_y}

    case CachingService.write_character(new_char) do
      {:ok, new_char} ->
        broadcast_on_map(new_char, MapViews.render(:mv, new_char), false)
        {:ok, new_char}

      {:error, _} = x ->
        x
    end
  end

  ## Private functions

  @spec broadcast_on_map(Character.t(), any, boolean) :: :ok
  defp broadcast_on_map(%Character{} = character, packet, including_self \\ true) do
    guards = if including_self, do: [], else: [{:!==, :id, character.id}]
    %Position{map_id: map_id} = Character.get_position(character)
    {:ok, players} = CachingService.get_characters_by_map_id(map_id, guards)
    Enum.each(players, &Socket.send(&1.socket, packet))
  end

  @spec send_visibility_packets(Character.t(), Character.t()) :: :ok | {:error, atom}
  defp send_visibility_packets(self, character) do
    Socket.send(self.socket, VisibilityViews.render(:in, character))
    Socket.send(character.socket, VisibilityViews.render(:in, self))
  end
end