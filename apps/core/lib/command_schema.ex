defmodule Core.CommandSchema do
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

    quote do
      def parse_packet_args([unquote(cmdname) | args], _socket) do
        {:ok, {unquote(cmdname), args}}
      end

      def resolve(unquote(cmdname), args, socket) do
        say_attrs = %{
          entity_type: :character,
          entity_id: socket.assigns.character_id,
          color: :special_gold,
          message: "#{unquote(@console_prefix)} #{unquote(cmdname)} #{Enum.join(args, " ")}"
        }

        render = ChannelEndpoint.Endpoint.ChatViews.render(:say, say_attrs)
        :ok = Core.Socket.send(socket, render)

        unquote(module).handle_command(unquote(cmdname), args, socket)
      end
    end
  end
end
