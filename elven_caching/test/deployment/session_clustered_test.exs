defmodule ElvenCaching.SessionClusteredTest do
  use ExUnit.ClusteredCase, async: false

  import AccountCase, only: [random_string: 0, session_attrs_mock: 1]

  alias ElvenCaching.MnesiaClusterManager
  alias ElvenCaching.SessionRegistry

  @opts [
    boot_timeout: System.get_env("BOOT_TIMEOUT", "10000") |> String.to_integer(),
    cluster_size: 2
  ]

  @moduletag :deployment

  ## Setup

  setup_all do
    # Start distribution
    {_, 0} = System.cmd("epmd", ["-daemon"])
    Node.start(:"primary@127.0.0.1", :longnames)
    :ok
  end

  setup do
    {:ok, id: random_string()}
  end

  ## Tests

  # A scenario defines a group of tests which will be run against a single cluster,
  # which is dynamically created. There are several options you can provide to configure
  # the cluster, including size, partitions, configuration, system environment and more.
  scenario "healthy cluster", @opts do
    node_setup(:config_node)

    # Just plain old tests - note the :cluster key of the context, which is needed to talk
    # to the nodes of the cluster via the Cluster API (an alias added for you)
    test "writes are replicated to all nodes", %{cluster: c, id: id} do
      sync_cluster(c)
      assert [error: :not_found, error: :not_found] = cluster_get(c, id)

      writer = Cluster.random_member(c)
      assert {:ok, _} = create_session(writer, id)
      assert [{:ok, _}, {:ok, _}] = cluster_get(c, id)

      writer = Cluster.random_member(c)
      assert {:ok, _} = delete_session(writer, id)
      assert [error: :not_found, error: :not_found] = cluster_get(c, id)
    end
  end

  ## Helpers

  def config_node(_) do
    Application.ensure_started(:mnesia)

    {:ok, manager} = GenServer.start(MnesiaClusterManager, nil, name: MnesiaClusterManager)
    RegistryTestHelpers.sync(manager)
    {:ok, registry} = GenServer.start(SessionRegistry, nil, name: SessionRegistry)
    RegistryTestHelpers.sync(registry)
  end

  defp sync_cluster(cluster) do
    Cluster.map(cluster, RegistryTestHelpers, :sync, [SessionRegistry])
  end

  defp cluster_get(cluster, id) do
    Cluster.map(cluster, SessionRegistry, :get, [id])
  end

  defp create_session(writer, id) do
    mock = session_attrs_mock(%{username: id})
    Cluster.call(writer, SessionRegistry, :create, [mock])
  end

  defp delete_session(writer, id) do
    Cluster.call(writer, SessionRegistry, :delete, [id])
  end
end
