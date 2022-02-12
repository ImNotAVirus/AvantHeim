defmodule ElvenViews.LoginPackets.FailcPacket do
  @moduledoc """
  TODO: Documentation.
  """

  use Core.SerializableStruct

  require ElvenViews.LoginPackets.FailcEnums

  alias __MODULE__
  alias ElvenViews.LoginPackets.FailcEnums

  defstruct error: nil

  @type t :: %FailcPacket{error: atom}

  @impl true
  def serialize(%FailcPacket{error: error}, _) do
    ["failc", FailcEnums.error(error || :generic, :value)]
  end
end
