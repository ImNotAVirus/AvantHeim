defmodule ChannelService.Endpoint.BankCommand do
  @moduledoc """
  TODO: Documentation.
  """

  alias ElvenCore.Socket
  alias ChannelService.Endpoint.ChatViews
  alias ChannelService.Endpoint.EntityInteractions
  alias ElvenCaching.Entity.Character
  alias ElvenCaching.CharacterRegistry

  ## Public API

  # > $bank
  # Usage: $bank <open|get|set|add|sub> [options]
  #
  # > $bank open
  # > [show bank widget in game]
  #
  # > $bank set test
  # Invalid value 'test'
  # Usage: $bank <set> [value:integer:0-5_000_000_000] [to] [player_name:string]
  #
  # > $bank set 5_000_000_001
  # DarkyZ has now 5_000_000_000 golds in bank
  #
  # > $bank set 666
  # DarkyZ has now 666 golds in bank
  #
  # > $bank add 3
  # DarkyZ has now 669 golds in bank
  #
  # > $bank sub 3
  # DarkyZ has now 666 golds in bank
  #
  # > $bank set -1
  # DarkyZ has now 0 golds in bank
  #
  # > $bank set 0 to UnknowPlayer
  # UnknowPlayer is not logged
  #
  # > $bank set 3 to Fizo
  # Fizo has now 3 golds in bank
  #
  # > $bank get
  # Current DarkyZ's bank gold: 3 golds
  #
  # > $bank get from Fizo
  # > Current Fizo's bank gold: 3 golds
  #
  # > $bank add 3 to Fizo
  # > Fizo has now 6 golds in bank
  #
  # > $bank sub 6 to Fizo
  # > Fizo has now 0 golds in bank

  @op_types ["set", "add", "sub"]

  @spec handle_command(String.t(), [String.t()], Socket.t()) :: {:cont, Socket.t()}
  def handle_command("$bank", args, socket) do
    %{character_id: character_id} = socket.assigns
    {:ok, character} = CharacterRegistry.get(character_id)

    case args do
      ["open"] ->
        EntityInteractions.open_bank_window(character)

      ["get"] ->
        get_bank_golds(socket, character, args, character)

      ["get", "from", name] ->
        apply_on_character(socket, character, args, name, &get_bank_golds/4)

      [op_type, _] = args when op_type in @op_types ->
        update_bank_golds(socket, character, args, character)

      [op_type, _, "to", name] = args when op_type in @op_types ->
        apply_on_character(socket, character, args, name, &update_bank_golds/4)

      args ->
        send_message(socket, character, usage(args), :special_red)
    end

    {:cont, socket}
  end

  ## Private functions

  @spec usage([String.t()]) :: String.t()
  defp usage(args) do
    msg =
      case args do
        ["open" | _] -> "open"
        ["get" | _] -> "get [from] [player_name:string]"
        ["set" | _] -> "set [value:integer:0-5_000_000_000] [to] [player_name:string]"
        ["add" | _] -> "add [value:integer:0-5_000_000_000] [to] [player_name:string]"
        ["sub" | _] -> "sub [value:integer:0-5_000_000_000] [to] [player_name:string]"
        _ -> "<open|get|set|add|sub> [options]"
      end

    "Usage: $bank " <> msg
  end

  @typep callback ::
           (Socket.t(), Character.t(), [String.t()], Character.t() -> :ok | {:error, atom})
  @spec apply_on_character(Socket.t(), Character.t(), [String.t()], String.t(), callback) ::
          :ok | {:error, atom}
  defp apply_on_character(socket, character, args, name, callback) do
    case CharacterRegistry.get_by_name(name) do
      {:ok, target} ->
        callback.(socket, character, args, target)

      {:error, :not_found} ->
        send_message(socket, character, "#{name} is not logged", :special_red)
    end
  end

  @spec get_bank_golds(Socket.t(), Character.t(), [String.t()], Character.t()) ::
          :ok | {:error, atom}
  defp get_bank_golds(socket, character, _args, target) do
    send_message(
      socket,
      character,
      "Current #{target.name}'s bank gold: #{ElvenCore.format_number(target.bank_gold)} golds",
      :special_green
    )
  end

  @spec update_bank_golds(Socket.t(), Character.t(), [String.t()], Character.t()) ::
          :ok | {:error, atom}
  defp update_bank_golds(socket, character, [op_type, str_val | _] = args, target) do
    case Integer.parse(str_val) do
      {value, ""} ->
        updated_gold = compute_golds(op_type, target.bank_gold, value)
        {:ok, new_char} = EntityInteractions.set_bank_golds(target, updated_gold)

        send_message(
          socket,
          new_char,
          "#{new_char.name} has now #{ElvenCore.format_number(new_char.bank_gold)} golds in bank",
          :special_green
        )

      _ ->
        send_message(socket, character, "Invalid value '#{str_val}'", :special_red)
        send_message(socket, character, usage(args), :special_red)
    end
  end

  defp compute_golds("set", _golds, value), do: value
  defp compute_golds("add", golds, value), do: golds + value
  defp compute_golds("sub", golds, value), do: golds - value

  defp send_message(socket, character, msg, color) do
    render = ChatViews.render(:say, %{entity: character, color: color, message: msg})
    Socket.send(socket, render)
  end
end
