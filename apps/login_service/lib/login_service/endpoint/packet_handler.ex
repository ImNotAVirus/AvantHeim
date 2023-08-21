defmodule LoginService.Endpoint.PacketHandler do
  @moduledoc """
  TODO: LoginService.Endpoint.PacketHandler
  """

  @behaviour ElvenGard.Network.PacketHandler

  alias ElvenPackets.Client.LoginPackets.NoS0575

  def handle_packet(%NoS0575{} = packet, socket) do
    LoginService.AuthActions.login("NoS0575", packet, socket)
    {:cont, socket}
  end
end
