defmodule ElvenPackets.SubPackets.Login.NsTeST.Channel do
  @moduledoc false

  use ElvenGard.Network.Type

  alias __MODULE__

  @enforce_keys [:id, :world_id, :world_name, :ip, :port, :population]
  defstruct @enforce_keys

  @type t :: %Channel{
          id: non_neg_integer,
          world_id: non_neg_integer,
          world_name: String.t(),
          ip: String.t(),
          port: :inet.port_number(),
          population: non_neg_integer
        }

  @impl true
  @spec encode(t(), Keyword.t()) :: binary()
  def encode(%Channel{} = struct, _) do
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

  @impl true
  def decode(_data, _opts) do
    # We do this to trick Dialyzer to not complain about non-local returns.
    case :erlang.phash2(1, 1) do
      0 -> raise("unimplemented decoder for #{inspect(__MODULE__)}")
      1 -> {nil, ""}
    end
  end
end
