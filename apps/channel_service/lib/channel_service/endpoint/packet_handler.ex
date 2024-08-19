defmodule ChannelService.Endpoint.PacketHandler do
  @moduledoc """
  TODO: ChannelService.Endpoint.PacketHandler
  """

  require Logger

  import ElvenGard.Network.Socket, only: [assign: 3]

  alias GameService.PlayerBundle
  alias GameService.Events, as: Evt

  alias ElvenPackets.Client.{
    AreaPackets,
    LobbyPackets
  }

  @behaviour ElvenGard.Network.PacketHandler

  @mix_env Mix.env()

  ## Special handlers without packet headers

  # First packet received: encryption key
  def handle_packet({:handshake, [enc_key]}, socket) when is_nil(socket.assigns.enc_key) do
    enc_key = String.to_integer(enc_key)

    if enc_key == 0 and @mix_env == :prod do
      Logger.warning("Encryption key is 0", socket_id: socket.id)
    end

    {:cont, assign(socket, :enc_key, enc_key)}
  end

  # Second packet received: username
  def handle_packet({:handshake, [username, "ORG", "0"]}, socket) do
    {:cont, assign(socket, :username, username)}
  end

  # Third packet received: password
  def handle_packet({:handshake, [password]}, socket) do
    params = %{username: socket.assigns.username, password: password}

    case ChannelService.AuthActions.handshake(:handshake, params, socket) do
      {:cont, socket} -> {:cont, assign(socket, :state, :ok)}
      {:halt, _} = result -> result
    end
  end

  ## Lobby handlers

  def handle_packet(%LobbyPackets.Select{} = packet, socket) do
    ChannelService.LobbyActions.select_character("select", packet, socket)
  end

  def handle_packet(%LobbyPackets.GameStart{} = packet, socket) do
    {:ok, bundle} = ChannelService.GameStartActions.game_start("game_start", packet, socket)

    new_socket =
      socket
      |> Map.update!(:assigns, &Map.delete(&1, :character))
      |> assign(:map_ref, PlayerBundle.map_ref(bundle))

    {:cont, new_socket}
  end

  ## Map handlers

  def handle_packet(%AreaPackets.Dir{} = packet, socket) do
    %AreaPackets.Dir{
      entity_type: entity_type,
      entity_id: entity_id,
      direction: direction
    } = packet

    # Send dir to the game engine
    {:ok, _events} =
      ElvenGard.ECS.push(
        %Evt.EntityChangeDirection{
          entity_type: entity_type,
          entity_id: entity_id,
          value: direction
        },
        partition: socket.assigns.map_ref
      )

    {:cont, socket}
  end

  def handle_packet(%AreaPackets.Walk{} = packet, socket) do
    %AreaPackets.Walk{pos_x: pos_x, pos_y: pos_y, speed: speed, checksum: checksum} = packet

    # Send walk to the game engine
    {:ok, _events} =
      ElvenGard.ECS.push(
        %Evt.EntityMove{
          entity_type: :player,
          entity_id: socket.assigns.character_id,
          pos_x: pos_x,
          pos_y: pos_y,
          speed: speed,
          checksum: checksum
        },
        partition: socket.assigns.map_ref
      )

    {:cont, socket}
  end

  # TODO: Not handled yet
  def handle_packet(%AreaPackets.Say{} = packet, socket) do
    %AreaPackets.Say{message: message} = packet

    {:ok, _events} =
      ElvenGard.ECS.push(
        %Evt.EntityMessage{
          entity_type: :player,
          entity_id: socket.assigns.character_id,
          scope: :map,
          message: message
        },
        partition: socket.assigns.map_ref
      )

    {:cont, socket}
  end

  # TODO: Not handled yet
  def handle_packet(%AreaPackets.Ncif{} = packet, socket) do
    %AreaPackets.Ncif{entity_type: entity_type, entity_id: entity_id} = packet

    {:ok, _events} =
      ElvenGard.ECS.push(
        %Evt.EntityInfoRequest{
          entity_type: :player,
          entity_id: socket.assigns.character_id,
          target_type: entity_type,
          target_id: entity_id
        },
        partition: socket.assigns.map_ref
      )

    {:cont, socket}
  end

  def handle_packet(%AreaPackets.Preq{}, socket) do
    {:ok, _events} =
      ElvenGard.ECS.push(
        %Evt.UsePortalRequest{player_id: socket.assigns.character_id},
        partition: socket.assigns.map_ref
      )

    {:cont, socket}
  end

  # Display the emote on the map
  # FIXME: Emote for other than the current player is not supported yet
  # TODO: Not handled yet
  def handle_packet(
        %AreaPackets.Guri{type: :emoji, entity_type: :player, entity_id: entity_id} = packet,
        socket
      )
      when entity_id == socket.assigns.character_id do
    %AreaPackets.Guri{guri_data: emote_id} = packet

    with {:ok, effect_id} <- emote_to_effect_id(emote_id) do
      {:ok, _events} =
        ElvenGard.ECS.push(
          %Evt.MapEffect{
            entity_type: :player,
            entity_id: entity_id,
            effect_id: effect_id
          },
          partition: socket.assigns.map_ref
        )
    else
      _ -> Logger.warning("invalid emote id: #{inspect(emote_id)}")
    end

    {:cont, socket}
  end

  ## Default handler

  def handle_packet(:ignore, socket), do: {:cont, socket}

  def handle_packet(packet, socket) do
    Logger.warning("unimplemented handler for #{inspect(packet)}")
    {:cont, socket}
  end

  ## Helpers

  @emote_offset 4099
  @rainbow_vomit_vnum 5116

  defp emote_to_effect_id(emote_id) do
    case emote_id do
      value when value in 973..999 -> {:ok, value + @emote_offset}
      1000 -> {:ok, @rainbow_vomit_vnum}
      _ -> {:error, :invalid_emote}
    end
  end
end
