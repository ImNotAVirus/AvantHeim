defmodule CachingService.CharacterRegistry do
  @moduledoc """
  TODO: Documentation

  TODO: Clean this code: functions should no return `{:error, any}`
  Cf. Memento doc
  """

  use GenServer

  require Logger

  alias CachingService.Entity.Character

  ## Interfaces

  @spec init_character(map) :: {:ok, Character.t()} | {:error, any}
  def init_character(attrs) do
    attrs |> Character.new() |> write_character()
  end

  @spec write_character(Character.t()) :: {:ok, Character.t()} | {:error, any}
  def write_character(character) do
    Memento.transaction(fn -> Memento.Query.write(character) end)
  end

  @spec get_character_by_id(pos_integer) :: {:ok, Character.t()} | {:error, :not_found}
  def get_character_by_id(id) do
    one(fn -> Memento.Query.read(Character, id) end)
  end

  @spec get_character_by_name(String.t()) :: {:ok, Character.t()} | {:error, :not_found}
  def get_character_by_name(name) do
    one(fn -> Memento.Query.select(Character, {:==, :name, name}) end)
  end

  @spec delete_character_by_account_id(pos_integer) :: {:ok, Character.t()} | {:error, :not_found}
  def delete_character_by_account_id(account_id) do
    one(fn ->
      case Memento.Query.select(Character, {:==, :account_id, account_id}) do
        [character] ->
          Memento.Query.delete_record(character)
          character

        [] ->
          Logger.warn("No character found with account id #{account_id}")
          nil
      end
    end)
  end

  @spec get_characters_by_map_id(pos_integer, [tuple]) :: {:ok, [Character.t()]} | {:error, any}
  def get_characters_by_map_id(map_id, except_guards \\ []) do
    guards = [{:==, :map_id, map_id} | List.wrap(except_guards)]
    Memento.transaction(fn -> Memento.Query.select(Character, guards) end)
  end

  ## Interfaces helpers

  defp one(query) do
    case Memento.transaction(query) do
      {:ok, value} when is_struct(value) -> {:ok, value}
      {:ok, [value]} -> {:ok, value}
      {:ok, []} -> {:error, :not_found}
      {:ok, nil} -> {:error, :not_found}
    end
  end

  ## Public API

  @spec start_link(any) :: :ignore | {:error, any} | {:ok, pid}
  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  ## GenServer behaviour

  @impl true
  def init(_) do
    Logger.info("CharacterRegistry starting...")
    {:ok, nil, {:continue, :init_mnesia}}
  end

  @impl true
  def handle_continue(:init_mnesia, nil) do
    Memento.Table.create!(CachingService.Entity.Character)
    :ok = Memento.wait([CachingService.Entity.Character])

    Logger.info("CharacterRegistry started")
    {:noreply, nil}
  end
end
