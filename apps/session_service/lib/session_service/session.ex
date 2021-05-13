defmodule SessionService.Session do
  @moduledoc """
  TODO: Documentation
  """

  alias SessionService.Session

  @enforce_keys [:id, :username, :password]
  defstruct @enforce_keys ++ [state: :authenticated, expire: :infinity, monitor: nil]

  @states [:authenticated, :in_lobby, :in_game, :disconnected]
  @default_ttl 120

  @type state :: :authenticated | :in_lobby | :in_game | :disconnected
  @type optional_reference :: reference | nil

  @type t ::
          %__MODULE__{
            id: pos_integer,
            username: String.t(),
            password: String.t(),
            state: state,
            expire: timeout,
            monitor: optional_reference
          }

  ## Public API

  @spec new(pos_integer, String.t(), String.t()) :: Session.t()
  def new(id, username, password) do
    %Session{
      id: id,
      username: username,
      password: password,
      expire: ttl_to_expire(@default_ttl)
    }
  end

  @spec in_channel?(Session.t()) :: boolean
  def in_channel?(%Session{} = session) do
    session.state in [:in_lobby, :in_game]
  end

  @doc """
  Return all valids states
  """
  @spec states() :: [atom, ...]
  def states(), do: @states

  @doc """
  Returns if the given state is valid of not
  Can be use as function guard.
  """
  defmacro is_valid_state(state) do
    quote do: unquote(state) in unquote(@states)
  end

  ## Private functions

  def ttl_to_expire(ttl) do
    case ttl do
      :infinity -> :infinity
      _ -> DateTime.to_unix(DateTime.utc_now()) + ttl
    end
  end
end
