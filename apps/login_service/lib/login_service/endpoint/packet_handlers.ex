defmodule LoginService.Endpoint.PacketHandlers do
  @moduledoc """
  TODO: LoginService.Endpoint.PacketHandlers
  """

  alias ElvenPackets.Client.LoginPackets.NoS0575

  def handle_packet(%NoS0575{} = packet, socket) do
    LoginService.Endpoint.AuthActions.login("NoS0575", packet, socket)
    {:cont, socket}
  end
end
