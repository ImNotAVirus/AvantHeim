defmodule CachingService.CharacterRegistry do
  @moduledoc """
  TODO: Documentation

  TODO: Clean this code: functions should no return `{:error, any}`
  Cf. Memento doc
  """

  use GenServer

  require Logger

  alias ElvenCore.Socket
  alias CachingService.Player.Character

  ## Public API

  @spec start_link(any) :: :ignore | {:error, any} | {:ok, pid}
  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  ## Interfaces

  @spec init_character(map, Socket.t()) :: {:ok, Character.t()} | {:error, any}
  def init_character(attrs, %Socket{} = socket) do
    Memento.transaction(fn ->
      attrs |> Character.new(socket) |> Memento.Query.write()
    end)
  end

  @spec write_character(Character.t()) :: {:ok, Character.t()} | {:error, any}
  def write_character(character) do
    Memento.transaction(fn -> Memento.Query.write(character) end)
  end

  @spec get_character_by_id(pos_integer) :: {:ok, Character.t()} | {:error, any}
  def get_character_by_id(id) do
    Memento.transaction(fn -> Memento.Query.read(Character, id) end)
  end

  @spec delete_character_by_id(pos_integer) :: {:ok, Character.t()} | {:error, any}
  def delete_character_by_id(id) do
    Memento.transaction(fn -> Memento.Query.delete(Character, id) end)
  end

  @spec get_character_by_name(String.t()) :: {:ok, Character.t()} | {:ok, nil}
  def get_character_by_name(name) do
    res = Memento.transaction(fn -> Memento.Query.select(Character, {:==, :name, name}) end)

    case res do
      {:ok, []} -> {:ok, nil}
      {:ok, [character]} -> {:ok, character}
    end
  end

  @spec get_characters_by_map_id(pos_integer, [tuple]) :: {:ok, [Character.t()]} | {:error, any}
  def get_characters_by_map_id(map_id, except_guards \\ []) do
    guards = [{:==, :map_id, map_id} | except_guards]
    Memento.transaction(fn -> Memento.Query.select(Character, guards) end)
  end

  ## GenServer behaviour

  @impl true
  def init(_) do
    Logger.info("CharacterRegistry starting...")
    {:ok, nil, {:continue, :init_mnesia}}
  end

  @impl true
  def handle_continue(:init_mnesia, nil) do
    Memento.Table.create!(CachingService.Player.Character)
    :ok = Memento.wait([CachingService.Player.Character])

    Logger.info("CharacterRegistry started")
    {:noreply, nil}
  end
end
