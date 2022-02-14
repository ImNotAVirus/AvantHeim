defmodule CachingService.Account.SessionTest do
  use AccountCase, async: true

  alias CachingService.Account.Session

  ## Tests

  describe "guard is_logged/1" do
    test "is defined" do
      require Session
      assert Session.is_logged(session_mock()) == false
      assert Session.is_logged(logged_session()) == true
    end
  end

  describe "new/1" do
    test "create a new structure" do
      assert %Session{} = Session.new(session_attrs_mock())
    end

    test "raises if a attribute is missing" do
      assert_raise ArgumentError, "missing attributes: [:username]", fn ->
        session_attrs_mock()
        |> Map.delete(:username)
        |> Session.new()
      end

      assert_raise ArgumentError, "missing attributes: [:username, :password]", fn ->
        session_attrs_mock()
        |> Map.delete(:username)
        |> Map.delete(:password)
        |> Session.new()
      end
    end
  end

  describe "set_ttl/2" do
    test "set integer expire" do
      assert %Session{expire: expire} = Session.set_ttl(session_without_expire(), 100)
      assert is_integer(expire)
    end

    test "set :infinity expire" do
      assert %Session{expire: expire} = Session.set_ttl(session_without_expire(), :infinity)
      assert expire == :infinity
    end
  end

  describe "set_state/2" do
    test "set state if valid" do
      assert %Session{state: state} = Session.set_state(session_without_state(), :saving)
      assert state == :saving
    end

    test "raises if state in invalid" do
      assert_raise FunctionClauseError, fn ->
        Session.set_state(session_without_state(), :invalid)
      end
    end
  end

  ## helpers

  defp logged_session() do
    %Session{session_mock() | state: :in_game}
  end

  defp session_without_expire() do
    %Session{session_mock() | expire: nil}
  end

  defp session_without_state() do
    %Session{session_mock() | state: nil}
  end
end
