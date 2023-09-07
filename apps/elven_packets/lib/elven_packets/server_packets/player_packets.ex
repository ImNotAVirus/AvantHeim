defmodule ElvenPackets.Server.PlayerPackets do
  @moduledoc """
  TODO: ElvenPackets.Server.PlayerPackets
  """

  use ElvenGard.Network.PacketSerializer

  import ElvenEnums.PlayerEnums,
    only: [
      authority: 1,
      gender: 1,
      hair_style: 1,
      hair_color: 1,
      character_class: 1,
      faction: 1,
      dignity_icon: 1,
      reputation_icon: 1,
      morph: 1,
      wings_design: 1
    ]

  alias ElvenPackets.SubPackets.Player.CInfo.Family
  alias ElvenPackets.Types.{NsInteger, NsString, NsEnum, NsBoolean}

  #######
  # Reputation display
  # ---
  # Example: "c_info Fizo - 1 1.915 family 12 1 1 1 1 1 1 1 0 1 1 1 0 0"
  #######
  @serializable true
  defpacket "c_info", as: CInfo do
    field(:name, NsString)
    field(:vnum, NsString, default: "-")
    field(:group_id, NsInteger)
    field(:family, Family)
    field(:character_id, NsInteger)
    field(:authority, NsEnum, values: authority(:__enumerators__))
    field(:gender, NsEnum, values: gender(:__enumerators__))
    field(:hair_style, NsEnum, values: hair_style(:__enumerators__))
    field(:hair_color, NsEnum, values: hair_color(:__enumerators__))
    field(:class, NsEnum, values: character_class(:__enumerators__))
    field(:reputation_icon, NsEnum, values: reputation_icon(:__enumerators__))
    field(:compliment, NsInteger)
    field(:morph, NsEnum, default: :default, values: morph(:__enumerators__))
    field(:is_invisible, NsBoolean)
    field(:family_level, NsInteger)
    field(:morph_upgrade, NsInteger)
    field(:wings_design, NsEnum, default: :default, values: wings_design(:__enumerators__))
    field(:is_arena_winner, NsBoolean)
  end

  #######
  # Reputation display
  # ---
  # Example: "fd 2 3 5 1"
  #######
  @serializable true
  defpacket "fd", as: Fd do
    field(:reputation, NsInteger)
    field(:reputation_icon, NsEnum, values: reputation_icon(:__enumerators__))
    field(:dignity, NsInteger)
    field(:dignity_icon, NsEnum, values: dignity_icon(:__enumerators__))
  end

  #######
  # Faction display
  # ---
  # Example: "fs 1"
  #######
  @serializable true
  defpacket "fs", as: Fs do
    field(:faction, NsEnum, values: faction(:__enumerators__))
  end

  #######
  # Character information display about levels
  # ---
  # Example: "lev 1 28319 1 1321 1980918 31289031 50000 1 10 1 100 0"
  #######
  @serializable true
  defpacket "lev", as: Lev do
    field(:level, NsInteger)
    field(:level_xp, NsInteger)
    field(:job_level, NsInteger)
    field(:job_level_xp, NsInteger)
    field(:level_xp_max, NsInteger)
    field(:job_level_xp_max, NsInteger)
    field(:reputation, NsInteger)
    field(:cp, NsInteger)
    field(:hero_level_xp, NsInteger)
    field(:hero_level, NsInteger)
    field(:hero_level_xp_max, NsInteger)
    field(:unknown, NsInteger, default: 0)
  end

  #######
  # Display completed act
  # ---
  # Example: "rsfi 1 1 0 0 1 1"
  #######
  @serializable true
  defpacket "rsfi", as: Rsfi do
    field(:act, NsInteger)
    field(:act_part, NsInteger)
    field(:unknown, NsInteger, default: 0)
    field(:unknown2, NsInteger, default: 0)
    field(:ts, NsInteger)
    field(:ts_max, NsInteger)
  end

  #######
  # Show hp/mp bar of player
  # ---
  # Example: "stat 201 201 150 150 0 1"
  #######
  @serializable true
  defpacket "stat", as: Stat do
    field(:hp, NsInteger)
    field(:hp_max, NsInteger)
    field(:mp, NsInteger)
    field(:mp_max, NsInteger)
    field(:unknown, NsInteger, default: 0)
    field(:option, NsInteger)
  end

  #######
  # Display title
  # ---
  # Example: "tit 1 Fizo"
  #######
  @serializable true
  defpacket "tit", as: Tit do
    field(:class, NsEnum, values: character_class(:__enumerators__), apply: &i18n/1)
    field(:name, NsString)
  end

  ## Private function

  def i18n(enum) do
    enum
    |> character_class()
    |> enum_to_i18n_key()
    |> PacketConstString.new!()
    |> Map.fetch!(:value)
  end

  # i18n value: 35
  defp enum_to_i18n_key(:adventurer), do: "Adventurer"
  # i18n value: 36
  defp enum_to_i18n_key(:swordman), do: "Swordsman"
  # i18n value: 37
  defp enum_to_i18n_key(:archer), do: "Archer"
  # i18n value: 38
  defp enum_to_i18n_key(:magician), do: "Mage"
  # i18n value: 39
  defp enum_to_i18n_key(:martial_artist), do: "MartialArtist"
end
