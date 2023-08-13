defmodule PresenceService.RegistryTest do
  use ExUnit.Case, async: true

  alias PresenceService.Session

  @registry {:global, PresenceService.Registry}

  ## Tests

  test "registry is started globally" do
    {:global, name} = @registry
    assert is_pid(:global.whereis_name(name))
  end

  describe "track/2" do
    test "save track of the session" do
      session = mock_session()

      assert :ok = PresenceService.Registry.track(self(), session)
      assert registry_state().id_mapping[session.account_id] != nil
      assert registry_state().name_mapping[session.username] != nil
    end

    # test "is monitoring the pid" do
    #   id = rand_int()
    #   name = rand_str()

    #   assert :ok = PresenceService.Registry.track(self(), id, name)
    #   assert self() in registry_monitors()
    # end

    # test "send an event to the PubSub when a user disconnect" do
    #   :ok = ElvenPubSub.subscribe(ElvenPubSub.Topics.session())

    #   id = rand_int()
    #   name = rand_str()
    #   {:ok, pid} = Task.start(fn -> :ok end)

    #   assert :ok = PresenceService.Registry.track(pid, id, name)
    #   assert_receive {:disconnected, {^id, ^name}}
    # end

    # test "remove the track in state after disconnection" do
    #   id = rand_int()
    #   name = rand_str()
    #   {:ok, pid} = Task.start(fn -> :ok end)

    #   assert :ok = PresenceService.Registry.track(pid, id, name)
    #   assert registry_state().id_mapping[id] == nil
    #   assert registry_state().name_mapping[name] == nil
    # end
  end

  ## Helpers

  defp registry_pid(), do: @registry |> elem(1) |> :global.whereis_name()
  defp registry_state(), do: :sys.get_state(@registry)
  defp rand_str(), do: :crypto.strong_rand_bytes(10) |> Base.encode16()
  defp rand_int(), do: Enum.random(1..999_999)

  defp mock_session() do
    Session.new(%{
      username: rand_str(),
      account_id: rand_int(),
      password: "",
      encryption_key: 0
    })
  end

  defp registry_monitors() do
    registry_pid()
    |> Process.info(:monitors)
    |> elem(1)
    |> Enum.map(&elem(&1, 1))
  end
end
