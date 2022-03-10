defmodule MapService.ConfigFile.MapPortal do
  @moduledoc false

  import ElvenEnums.MapEnums, only: [portal_type: 1]

  alias __MODULE__
  alias ElvenEnums.MapEnums

  @enforce_keys [
    :destination_map_id,
    :destination_map_x,
    :destination_map_y,
    :source_map_id,
    :source_map_x,
    :source_map_y,
    :type
  ]
  defstruct @enforce_keys

  @type t :: %MapPortal{
          destination_map_id: non_neg_integer,
          destination_map_x: non_neg_integer,
          destination_map_y: non_neg_integer,
          source_map_id: non_neg_integer,
          source_map_x: non_neg_integer,
          source_map_y: non_neg_integer,
          type: MapEnums.portal_type_keys()
        }

  ## Public API

  def new(config) do
    %MapPortal{
      destination_map_id: Map.fetch!(config, "destination_map_id"),
      destination_map_x: Map.fetch!(config, "destination_map_x"),
      destination_map_y: Map.fetch!(config, "destination_map_y"),
      source_map_id: Map.fetch!(config, "source_map_id"),
      source_map_x: Map.fetch!(config, "source_map_x"),
      source_map_y: Map.fetch!(config, "source_map_y"),
      type: fetch_portal_type(config)
    }
  end

  ## Private functions

  defp fetch_portal_type(config) do
    type = Map.fetch!(config, "type")

    if type not in portal_type(:__keys__) do
      raise "unknown portal type #{inspect(type)}"
    end

    type
  end
end
