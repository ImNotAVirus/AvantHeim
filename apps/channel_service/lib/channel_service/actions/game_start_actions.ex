defmodule ChannelService.GameStartActions do
  @moduledoc """
  TODO: Documentation
  """

  alias ElvenGard.Network.Socket
  alias ElvenGard.ECS.Command
  alias GameService.PlayerBundle
  alias GameService.Events.EntityMapEnter

  alias ElvenPackets.Views.{
    ChatViews,
    PlayerViews,
    UIViews
  }

  ## Packet handlers

  @spec game_start(String.t(), map, Socket.t()) :: {:ok, PlayerBundle.t()}
  def game_start("game_start", _, %Socket{} = socket) do
    %{character: player, account: account} = socket.assigns

    # Spawn Player entity into system
    specs = PlayerBundle.specs(player, account, self())
    {:ok, {entity, components}} = Command.spawn_entity(specs)
    bundle = GameService.load_bundle(entity, components)

    # Send game start packets
    _ = send_tit(socket, bundle)
    _ = send_fd(socket, bundle)
    # TODO: Socket.send(socket, PlayerViews.render(:ski, %{entity: bundle}))
    _ = Socket.send(socket, PlayerViews.render(:rsfi))
    _ = send_fs(socket, bundle)
    _ = send_gold(socket, bundle)
    # TODO: Socket.send(socket, InventoryViews.render(:qslot, %{slot_id: 0, entity: bundle}))
    # TODO: Socket.send(socket, InventoryViews.render(:qslot, %{slot_id: 1, entity: bundle}))
    Socket.send(socket, UIViews.render(:info, %{message: "Welcome to my World!"}))
    _ = send_bns(socket)
    _ = send_hello(socket, bundle)

    # Send an entity map enter event to the map partition
    {:ok, _events} =
      ElvenGard.ECS.push(
        %EntityMapEnter{entity_type: :player, entity_id: player.id},
        partition: PlayerBundle.map_ref(bundle)
      )

    {:ok, bundle}
  end

  ## Helpers

  defp send_tit(socket, bundle) do
    attrs = %{
      class: PlayerBundle.class(bundle),
      name: PlayerBundle.name(bundle)
    }

    Socket.send(socket, PlayerViews.render(:tit, attrs))
  end

  defp send_fd(socket, bundle) do
    attrs = %{
      reputation: PlayerBundle.reputation(bundle),
      reputation_icon: PlayerBundle.reputation_icon(bundle),
      dignity: PlayerBundle.dignity(bundle),
      dignity_icon: PlayerBundle.dignity_icon(bundle)
    }

    Socket.send(socket, PlayerViews.render(:fd, attrs))
  end

  defp send_fs(socket, bundle) do
    attrs = %{faction: PlayerBundle.faction(bundle)}
    Socket.send(socket, PlayerViews.render(:fs, attrs))
  end

  defp send_gold(socket, bundle) do
    attrs = %{
      gold: PlayerBundle.gold(bundle),
      bank_gold: PlayerBundle.bank_gold(bundle)
    }

    Socket.send(socket, UIViews.render(:gold, attrs))
  end

  defp send_bns(socket) do
    messages = Enum.map(1..10, fn x -> "ElvenGard ##{x}" end)

    messages
    |> Enum.with_index()
    |> Stream.map(fn {val, i} -> %{id: i, message: val} end)
    |> Enum.each(&Socket.send(socket, ChatViews.render(:bn, &1)))
  end

  defp send_hello(socket, player) do
    prefix = String.duplicate("-", 31)
    suffix = String.duplicate("-", 82)

    messages = [
      {:special_green, "#{prefix} [ ElvenGard ] #{prefix}"},
      {:special_red, "Github: https://github.com/ImNotAVirus/AvantHeim"},
      {:special_red, "Author: DarkyZ aka. ImNotAVirus"},
      {:special_green, suffix}
    ]

    Enum.each(messages, fn {color, message} ->
      attrs = %{entity_type: :player, entity_id: player.id, color: color, message: message}
      Socket.send(socket, ChatViews.render(:say, attrs))
    end)
  end
end
