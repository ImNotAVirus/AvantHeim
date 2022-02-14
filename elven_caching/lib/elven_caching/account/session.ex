defmodule ElvenCaching.Account.Session do
  @moduledoc """
  TODO: Documentation
  """

  alias __MODULE__

  @required_attributes [:username, :password, :account_id, :encryption_key]
  @virtual_attributes %{expire: nil, state: :authenticated}
  @states [:authenticated, :in_lobby, :in_game, :saving]
  @default_ttl Application.get_env(:elven_caching, :session_ttl, 120)

  use Memento.Table,
    attributes: @required_attributes ++ Map.keys(@virtual_attributes),
    type: :ordered_set

  @type state :: :authenticated | :in_lobby | :in_game | :saving

  @type t :: %Session{
          username: String.t(),
          password: String.t(),
          account_id: pos_integer,
          encryption_key: pos_integer,
          expire: timeout,
          state: state
        }

  ## Public API

  defguard is_logged(s) when is_struct(s, Session) and s.state in [:in_lobby, :in_game, :saving]

  @spec new(map) :: t()
  def new(attrs) do
    default = %{expire: ttl_to_expire(@default_ttl)}

    attrs
    |> extract_attributes!(@required_attributes)
    |> Map.merge(@virtual_attributes)
    |> Map.merge(default)
    |> then(&struct!(Session, &1))
  end

  @spec set_ttl(t(), timeout) :: t()
  def set_ttl(%Session{} = session, ttl) do
    %Session{session | expire: ttl_to_expire(ttl)}
  end

  @spec set_state(t(), atom) :: t()
  def set_state(%Session{} = session, state) when state in @states do
    %Session{session | state: state}
  end

  ## Internal helpers

  @spec ttl_to_expire(timeout) :: timeout
  def ttl_to_expire(ttl) do
    case ttl do
      :infinity -> :infinity
      _ -> System.monotonic_time(:millisecond) + ttl
    end
  end

  ## Private functions

  defp extract_attributes!(attrs, required_attributes) do
    case required_attributes -- Map.keys(attrs) do
      [] -> Map.take(attrs, required_attributes)
      missing -> raise ArgumentError, "missing attributes: #{inspect(missing)}"
    end
  end
end
