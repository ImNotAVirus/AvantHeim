defmodule PresenceService.SessionProcessTest do
  use ExUnit.Case, async: true

  alias PresenceService.Session
  alias PresenceService.SessionProcess

  ## Tests

  test "exits after init_timeout" do
    childspec = SessionProcess.child_spec({mock_session(), [init_timeout: 10]})
    pid = start_supervised!(childspec)
    ref = Process.monitor(pid)

    assert_receive {:DOWN, ^ref, :process, ^pid, :normal}
  end

  test "doesn't exits when entering in authenticated state" do
    childspec = SessionProcess.child_spec({mock_session(), [init_timeout: 10]})
    pid = start_supervised!(childspec)
    ref = Process.monitor(pid)

    assert :ok = SessionProcess.authenticate(pid)
    refute_receive {:DOWN, ^ref, :process, ^pid, _}
  end

  ## Helpers

  defp rand_str(), do: :crypto.strong_rand_bytes(10) |> Base.encode16()
  defp rand_int(), do: Enum.random(1..999_999)

  defp mock_session() do
    Session.new(%{
      account_id: rand_int(),
      username: rand_str(),
      password: "",
      encryption_key: 0
    })
  end
end
