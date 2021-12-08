defmodule CachingService.Player.Session do
  @moduledoc """
  TODO: Documentation
  """

  alias __MODULE__

  @required_attributes [:username, :password, :encryption_key, :state, :expire]
  @states [:authenticated, :in_lobby, :in_game, :disconnected]
  @default_ttl 120
  @max_key_value 65535

  use Memento.Table,
    attributes: @required_attributes,
    index: [:encryption_key],
    type: :ordered_set

  @type state :: :authenticated | :in_lobby | :in_game | :disconnected

  @type t :: %Session{
          username: String.t(),
          password: String.t(),
          encryption_key: pos_integer,
          state: state,
          expire: timeout
        }

  ## Public API

  @typep maybe_session_key :: pos_integer | nil
  @spec new(String.t(), String.t(), maybe_session_key(), timeout) :: t()
  def new(username, password, encryption_key \\ nil, ttl \\ @default_ttl) do
    %Session{
      username: username,
      password: password,
      encryption_key: encryption_key || :rand.uniform(@max_key_value),
      state: :authenticated,
      expire: ttl_to_expire(ttl)
    }
  end

  @spec valid_states() :: [atom, ...]
  def valid_states(), do: @states

  defguard is_logged(s) when is_struct(s, Session) and s.state in [:in_lobby, :in_game]

  ## Internal helpers

  @spec ttl_to_expire(timeout) :: timeout
  def ttl_to_expire(ttl) do
    case ttl do
      :infinity -> :infinity
      _ -> DateTime.to_unix(DateTime.utc_now()) + ttl
    end
  end
end
