defmodule LoginService.Endpoint.PacketHandlers do
  @moduledoc """
  TODO: LoginService.Endpoint.PacketHandlers
  """

  alias LoginService.ClientPackets.NoS0575

  def handle_packet(%NoS0575{} = packet, socket) do
    LoginService.Endpoint.AuthActions.login(nil, packet, socket)
    {:cont, socket}
  end
end
