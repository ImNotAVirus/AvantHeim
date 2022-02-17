defmodule ChannelService.Endpoint.PlayerPackets.Tit do
  @moduledoc """
  TODO: Documentation.
  """

  use ElvenCore.SerializableStruct

  alias __MODULE__

  @enforce_keys [:class, :name]
  defstruct @enforce_keys

  @type t :: %Tit{class: atom, name: String.t()}

  @impl true
  def serialize(%Tit{} = struct, _) do
    %Tit{class: class, name: name} = struct
    "tit #{i18n_class(class)} #{name}"
  end

  ## Private functions

  defp i18n_class(:adventurer), do: 35
  defp i18n_class(:swordman), do: 36
  defp i18n_class(:archer), do: 37
  defp i18n_class(:magician), do: 38
  defp i18n_class(:martial_artist), do: 39
end
