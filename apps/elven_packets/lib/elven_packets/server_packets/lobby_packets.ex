defmodule ElvenPackets.Server.LobbyPackets do
  @moduledoc """
  TODO: ElvenPackets.Server.LobbyPackets
  """

  use ElvenGard.Network.PacketSerializer

  import ElvenEnums.PlayerEnums,
    only: [gender: 1, hair_style: 1, hair_color: 1, character_class: 1]

  alias ElvenPackets.SubPackets.Equipment
  alias ElvenPackets.Types.{NsEnum, NsInteger, NsList, NsString}

  ## Lobby packets

  @serializable true
  defpacket("OK", as: Ok)

  @serializable true
  defpacket "clist_start", as: CListStart do
    field(:unused, NsInteger, default: 0)
  end

  @serializable true
  defpacket("clist_end", as: CListEnd)

  @serializable true
  defpacket "clist", as: CList do
    field(:slot, NsInteger)
    field(:name, NsString)
    field(:unknown1, NsInteger, default: 0)
    field(:gender, NsEnum, values: gender(:__enumerators__))
    field(:hair_style, NsEnum, values: hair_style(:__enumerators__))
    field(:hair_color, NsEnum, values: hair_color(:__enumerators__))
    field(:unknown2, NsInteger, default: 0)
    field(:class, NsEnum, values: character_class(:__enumerators__))
    field(:level, NsInteger)
    field(:hero_level, NsInteger)
    field(:equipments, Equipment)
    field(:job_level, NsInteger)
    field(:quest_completion, NsInteger, default: 1)
    field(:quest_part, NsInteger, default: 1)
    field(:pets, NsList, type: NsInteger, joiner: ".")
    field(:design, NsInteger)
  end
end
