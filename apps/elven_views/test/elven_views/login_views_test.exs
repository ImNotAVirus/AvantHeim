defmodule ElvenViews.LoginViewsTest do
  use ViewCase, async: true

  alias ElvenViews.LoginViews

  alias ElvenViews.LoginPackets.{
    FailcPacket,
    NsTeSTPacket
  }

  alias ElvenViews.LoginPackets.NsTeST.Channel

  ## Tests

  describe "render:failc" do
    test "returns a packet structure" do
      mock = mock_failc()
      render = LoginViews.render(:failc, mock)

      assert %FailcPacket{} = render
      assert render.error == mock.error
    end

    test "error is optional" do
      mock = mock_failc(%{})
      render = LoginViews.render(:failc, mock)

      assert %FailcPacket{} = render
      assert is_nil(render.error)
    end
  end

  describe "render:nstest" do
    test "require username in args" do
      assert_raise ArgumentError, fn ->
        mock = Map.delete(mock_nstest(), :username)
        LoginViews.render(:nstest, mock)
      end
    end

    test "require encryption_key in args" do
      assert_raise ArgumentError, fn ->
        mock = Map.delete(mock_nstest(), :encryption_key)
        LoginViews.render(:nstest, mock)
      end
    end

    test "require ip in args" do
      assert_raise ArgumentError, fn ->
        mock = Map.delete(mock_nstest(), :ip)
        LoginViews.render(:nstest, mock)
      end
    end

    test "require port in args" do
      assert_raise ArgumentError, fn ->
        mock = Map.delete(mock_nstest(), :port)
        LoginViews.render(:nstest, mock)
      end
    end

    test "returns a packet structure" do
      mock = mock_nstest()
      render = LoginViews.render(:nstest, mock)

      assert %NsTeSTPacket{} = render
      assert render.encryption_key == mock.encryption_key
      assert render.username == mock.username
      assert [%Channel{ip: ip, port: port}] = render.server_list
      assert ip == "127.0.0.1"
      assert port == 4000
    end
  end

  ## Helpers

  defp mock_failc(attrs \\ %{error: :old_client}) do
    attrs
  end

  defp mock_nstest() do
    %{
      username: "admin",
      encryption_key: 123,
      ip: "127.0.0.1",
      port: 4000
    }
  end
end
