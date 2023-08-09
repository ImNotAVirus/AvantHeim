defmodule ElvenViews.SubPackets.FamilyIdRankSubPacket do
  @moduledoc false

  use ElvenCore.SerializableStruct

  alias __MODULE__
  alias ElvenI18n.PacketConstString

  @enforce_keys [:id, :rank]
  defstruct @enforce_keys

  @type t :: %FamilyIdRankSubPacket{
          id: integer,
          rank: PlayerEnums.family_rank_keys()
        }

  ## Public API

  @spec default() :: t()
  def default() do
    %FamilyIdRankSubPacket{id: -1, rank: :member}
  end

  ## SerializableStruct behaviour

  @impl true
  def serialize(%FamilyIdRankSubPacket{} = struct, _) do
    %FamilyIdRankSubPacket{id: id, rank: rank} = struct

    case id < 0 do
      true -> "-1"
      false -> "#{id}.#{i18n(rank)}"
    end
  end

  ## Private helpers

  defp i18n(enum) do
    enum
    |> enum_to_i18n_key()
    |> PacketConstString.new!()
    |> Map.fetch!(:value)
  end

  # i18n value: 915
  defp enum_to_i18n_key(:head), do: "Familyhead"
  # i18n value: 916
  defp enum_to_i18n_key(:deputy), do: "Familydeputy"
  # i18n value: 917
  defp enum_to_i18n_key(:keeper), do: "Familykeeper"
  # i18n value: 918
  defp enum_to_i18n_key(:member), do: "Member"
end
