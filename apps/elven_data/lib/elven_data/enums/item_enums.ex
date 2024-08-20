defmodule ElvenData.Enums.ItemEnums do
  @moduledoc """
  TODO: Documentation
  """

  import SimpleEnum, only: [defenum: 2]

  defenum :inventory_type,
    equipment: 0,
    main: 1,
    etc: 2,
    miniland: 3,
    specialist: 6,
    costume: 7
end
