defmodule CachingService.CharacterRegistry do
  @moduledoc """
  TODO: Documentation
  """

  use GenServer

  require Logger

  alias CachingService.Player.Character
  alias DatabaseService.Players.Character, as: DBCharacter

  ## Public API

  @spec start_link(any) :: :ignore | {:error, any} | {:ok, pid}
  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  ## Interfaces

  @spec init_character(DBCharacter.t()) :: {:ok, Character.t()} | {:error, any}
  def init_character(%DBCharacter{} = character) do
    Memento.transaction(fn ->
      character |> Character.new() |> Memento.Query.write()
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
