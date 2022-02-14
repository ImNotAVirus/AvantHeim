defmodule ElvenCaching.SessionRegistryTest do
  use AccountCase, async: true

  alias ElvenCaching.SessionRegistry
  alias ElvenCaching.Account.Session

  ## Setup

  setup_all do
    registry = start_supervised!(SessionRegistry)
    {:ok, registry: registry}
  end

  setup do
    {:ok, id: random_string()}
  end

  ## Tests

  describe "create/1" do
    test "creates a new Session", %{id: id} do
      assert [] = get_session(id)
      assert {:ok, %Session{}} = create_session(id)
      assert [_] = get_session(id)
    end

    test "replace a Session if not logged", %{id: id} do
      assert [] = get_session(id)

      assert {:ok, %Session{}} = create_session(id)
      assert [cached_session1] = get_session(id)

      assert {:ok, %Session{}} = create_session(id)
      assert [cached_session2] = get_session(id)

      assert attribute_index(cached_session1, 1) != attribute_index(cached_session2, 1)
    end

    test "returns an error if session is logged", %{id: id} do
      assert [] = get_session(id)
      assert {:ok, %Session{}} = create_session(id, %{state: :saving})
      assert {:error, :already_exists} = create_session(id)
    end
  end

  describe "write/1" do
    test "creates a new Session if not exists", %{id: id} do
      session = session_mock(%{username: id})

      assert [] = get_session(id)
      assert {:ok, ^session} = SessionRegistry.write(session)
      assert [_] = get_session(id)
    end

    test "updates an existing Session", %{id: id} do
      {:ok, session} = create_session(id)
      updated_session = %Session{session | password: "NewPassword"}

      assert [_] = get_session(id)
      assert {:ok, ^updated_session} = SessionRegistry.write(updated_session)
      assert [cached_session] = get_session(id)
      assert attribute_index(cached_session, 1) == "NewPassword"
    end
  end

  describe "get/1" do
    test "returns an existing session", %{id: id} do
      {:ok, session} = create_session(id)
      assert {:ok, ^session} = SessionRegistry.get(id)
    end

    test "returns :not_found if not existing", %{id: id} do
      assert {:error, :not_found} = SessionRegistry.get(id)
    end
  end

  describe "delete/1" do
    test "deletes an existing session", %{id: id} do
      {:ok, session} = create_session(id)

      assert [_] = get_session(id)
      assert {:ok, ^session} = SessionRegistry.delete(id)
      assert [] = get_session(id)
    end

    test "returns :not_found if not existing", %{id: id} do
      assert {:error, :not_found} = SessionRegistry.delete(id)
    end
  end

  ## Helpers

  defp create_session(id) do
    %{username: id} |> session_attrs_mock() |> SessionRegistry.create()
  end

  defp create_session(id, attrs) do
    %{username: id}
    |> session_mock()
    |> Map.merge(attrs)
    |> SessionRegistry.write()
  end

  defp get_session(id) do
    :mnesia.dirty_read({Session, id})
  end

  defp attribute_index(tuple, index) do
    # +1 because the first elem is the record name
    elem(tuple, index + 1)
  end
end
