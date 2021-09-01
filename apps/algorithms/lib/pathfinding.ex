defmodule Algorithms.Pathfinding do
  use Rustler, otp_app: :algorithms, crate: "pathfinding_nif"

  # Algorithms.Pathfinding.astar(<<0,0,0,0,0,0,1,0,1,0,0,1,0,0,0,0,1,1,0,0,0,0,0,0,0>>,5,5,{0,0},{4,4})
  @type coord :: {non_neg_integer, non_neg_integer}
  @spec astar(binary, pos_integer, pos_integer, coord, coord) :: {:ok, [coord]} | {:error, atom}
  def astar(_map, _width, _height, _start, _end), do: :erlang.nif_error(:nif_not_loaded)
end
