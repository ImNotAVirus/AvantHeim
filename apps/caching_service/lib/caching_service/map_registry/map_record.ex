defmodule CachingService.MapRegistry.MapRecord do
  @moduledoc """
  TODO: Documentation
  """

  import Record, only: [defrecord: 2]

  defrecord :map_record, [:id, :width, :height, :tensor]

  @type t ::
          record(:map_record,
            id: pos_integer,
            width: pos_integer,
            height: pos_integer,
            tensor: Nx.Tensor.t()
          )
end
