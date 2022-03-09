defmodule ElvenViews.LoginViewsTest do
  use ViewCase, async: true

  alias ElvenViews.LoginViews

  alias ElvenViews.LoginPackets.{
    FailcPacket,
    NsTeSTPacket
  }

  alias ElvenViews.LoginPackets.NsTeST.Channel

  ## Tests

  describe "render:login_error" do
    test "returns a packet structure" do
      assert %FailcPacket{error: :old_client} =
               LoginViews.render(:login_error, %{error: :old_client})
    end

    test "error is optional" do
      assert %FailcPacket{error: nil} = LoginViews.render(:login_error, %{})
    end
  end

  describe "render:login_succeed" do
    test "require username in args" do
      assert_raise ArgumentError, fn ->
        args = Map.delete(default_args(), :username)
        LoginViews.render(:login_succeed, args)
      end
    end

    test "require encryption_key in args" do
      assert_raise ArgumentError, fn ->
        args = Map.delete(default_args(), :encryption_key)
        LoginViews.render(:login_succeed, args)
      end
    end

    test "require ip in args" do
      assert_raise ArgumentError, fn ->
        args = Map.delete(default_args(), :ip)
        LoginViews.render(:login_succeed, args)
      end
    end

    test "require port in args" do
      assert_raise ArgumentError, fn ->
        args = Map.delete(default_args(), :port)
        LoginViews.render(:login_succeed, args)
      end
    end

    test "returns a packet structure" do
      assert render = LoginViews.render(:login_succeed, default_args())
      assert %NsTeSTPacket{} = render
      assert render.encryption_key == 123
      assert render.username == "admin"
      assert [%Channel{ip: ip, port: port}] = render.server_list
      assert ip == "127.0.0.1"
      assert port == 4000
    end
  end

  ## Helpers

  defp default_args() do
    %{username: "admin", encryption_key: 123, ip: "127.0.0.1", port: 4000}
  end
end
