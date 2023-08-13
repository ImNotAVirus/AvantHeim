defmodule ElvenPackets.Server.LoginPackets.Macros do
  @moduledoc false

  defmacro empty_server(), do: "-99 0 -99 0 -99 0 -99 0"
  defmacro unused_servers(), do: List.duplicate(empty_server(), 3)
end

defmodule ElvenPackets.Server.LoginPackets do
  @moduledoc """
  TODO: ElvenPackets.Server.LoginPackets
  """

  use ElvenGard.Network.PacketSerializer

  import ElvenPackets.Enums.LoginEnums, only: [failc_error: 1, login_region: 1, auth_type: 1]
  import ElvenPackets.Server.LoginPackets.Macros

  alias ElvenPackets.SubPackets.Login.NsTeST.Channel
  alias ElvenPackets.Types.{NsEnum, NsInteger, NsList, NsString}

  ## Login packets

  @serializable true
  defpacket "failc", as: Failc do
    field :error, NsEnum, default: :generic, values: failc_error(:__enumerators__)
  end

  @serializable true
  defpacket "NsTeST", as: NsTeST do
    field :region, NsEnum, values: login_region(:__enumerators__)
    field :username, NsString
    field :auth_type, NsEnum, default: :gf, values: auth_type(:__enumerators__)
    field :server1, NsString, default: empty_server()
    field :server2, NsString, default: empty_server()
    field :server3, NsString, default: empty_server()
    field :server4, NsString, default: empty_server()
    field :server5, NsString, default: empty_server()
    field :server6, NsString, default: empty_server()
    field :unused_servers, NsList, joiner: " ", default: unused_servers()
    field :unknown, NsInteger, default: 0
    field :encryption_key, NsInteger
    field :server_list, NsList, type: Channel, joiner: " "
    field :terminator, NsString, default: "-1:-1:-1:10000.10000.1"
  end
end
