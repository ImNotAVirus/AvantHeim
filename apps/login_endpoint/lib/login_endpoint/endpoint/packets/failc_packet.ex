defmodule LoginEndpoint.Endpoint.FailcPacket do
  @moduledoc """
  TODO: Documentation.
  """

  use Core.SerializableStruct

  alias __MODULE__

  defstruct error: nil

  @type t :: %FailcPacket{error: atom}

  @default_error 2
  @error_map %{
    old_client: 1,
    already_connected: 4,
    bad_credentials: 5
  }

  @impl true
  def serialize(%FailcPacket{error: error}, _) do
    "failc #{Map.get(@error_map, error, @default_error)}"
  end
end
