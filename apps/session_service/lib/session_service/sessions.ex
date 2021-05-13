defmodule SessionService.Sessions do
  @moduledoc """
  TODO: Documentation
  """

  require Logger

  alias SessionService.Session

  @table_name :sessions

  @type optional_session :: Session.t() | nil
  @type ets_ctx :: tuple

  ## Public API

  @spec init_ets() :: ets_ctx
  def init_ets() do
    table_pid = :ets.new(@table_name, [:set, :private])
    counter_ref = :counters.new(1, [])
    :ok = :counters.add(counter_ref, 1, 1)
    {table_pid, counter_ref}
  end

  @spec get_by_username(String.t(), ets_ctx) :: optional_session
  def get_by_username(username, {table_pid, _}) do
    case :ets.lookup(table_pid, username) do
      [] -> nil
      [{^username, session}] -> session
    end
  end

  @spec create(String.t(), String.t(), ets_ctx) :: Session.t()
  def create(username, password, {table_pid, counter_ref}) do
    id = :counters.get(counter_ref, 1)
    :ok = :counters.add(counter_ref, 1, 1)
    session = Session.new(id, username, password)
    true = :ets.insert(table_pid, {username, session})
    session
  end

  @spec clean_expired_keys(ets_ctx) :: any
  def clean_expired_keys({table_pid, _}) do
    curr_time = DateTime.to_unix(DateTime.utc_now())

    # :ets.fun2ms(fn {_, session} when session.expire < curr_time -> true end)
    query = [{{:_, :"$1"}, [{:<, {:map_get, :expire, :"$1"}, curr_time}], [true]}]
    num_deleted = :ets.select_delete(table_pid, query)

    Logger.debug("Table `#{@table_name}`: cleared #{num_deleted} key(s)")
  end
end
