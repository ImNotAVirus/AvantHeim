defmodule ChannelService.CommandSchema do
  @moduledoc """
  TODO: Documentation
  TODO: Clean this module
  """

  @command_prefix "$"
  @console_prefix ">"

  ## Public API

  @doc false
  defmacro __using__(_) do
    quote do
      import unquote(__MODULE__), only: :macros
    end
  end

  @doc """
  Define a new command handler
  """
  defmacro defcommand(name, module) do
    cmdname = @command_prefix <> name

    quote location: :keep do
      def parse({unquote(cmdname), bin_args}, _socket, _opts) do
        {:ok, {unquote(cmdname), bin_args}}
      end

      def resolve(unquote(cmdname), bin_args, socket) do
        say_attrs = %{
          entity_type: :player,
          entity_id: socket.assigns.character_id,
          color: :special_gold,
          message: "#{unquote(@console_prefix)} #{unquote(cmdname)} #{bin_args}"
        }

        # FIXME: Why the fck Core depend on ChannelService
        render = ElvenPackets.Views.ChatViews.render(:say, say_attrs)
        :ok = ElvenGard.Network.Socket.send(socket, render)

        args = String.split(bin_args, " ")
        unquote(module).handle_command(unquote(cmdname), args, socket)
      end
    end
  end
end
