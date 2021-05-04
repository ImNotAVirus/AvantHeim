defmodule LoginEndpoint.Endpoint.NsTeST.Channel do
  @moduledoc """
  TODO: Documentation
  """

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

  ## Public API

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

  ## Implementation

  defimpl Core.Socket.SerializerProtocol do
    def serialize(data, opts) do
      Channel.serialize(data, opts)
    end
  end
end
