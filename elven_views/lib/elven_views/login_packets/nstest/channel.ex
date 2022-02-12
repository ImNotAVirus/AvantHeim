defmodule ElvenViews.LoginPackets.NsTeST.Channel do
  @moduledoc """
  TODO: Documentation
  """

  use Core.SerializableStruct

  alias __MODULE__

  @enforce_keys [:id, :world_id, :world_name, :ip, :port, :population]
  defstruct @enforce_keys

  @type t :: %Channel{
          id: non_neg_integer,
          world_id: non_neg_integer,
          world_name: String.t(),
          ip: String.t(),
          port: 1..65535,
          population: non_neg_integer
        }

  @impl true
  def serialize(%Channel{} = struct, _) do
    %Channel{
      id: id,
      world_id: world_id,
      world_name: world_name,
      ip: ip,
      port: port,
      population: population
    } = struct

    "#{ip}:#{port}:#{population}:#{world_id}.#{id}.#{world_name}"
  end
end
