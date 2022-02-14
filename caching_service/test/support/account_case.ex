defmodule AccountCase do
  use ExUnit.CaseTemplate

  alias CachingService.Account.Session

  setup do
    # This will run before each test that uses this case
    :ok
  end

  using do
    quote do
      import AccountCase,
        only: [
          random_integer: 0,
          random_string: 0,
          session_mock: 0,
          session_mock: 1,
          session_attrs_mock: 0,
          session_attrs_mock: 1
        ]
    end
  end

  ## Helpers

  def random_integer() do
    System.unique_integer([:positive])
  end

  def random_string() do
    for _ <- 1..10, into: "", do: <<Enum.random('0123456789abcdef')>>
  end

  ## Mocks

  def session_mock(attrs \\ %{}) do
    attrs |> session_attrs_mock() |> Session.new()
  end

  def session_attrs_mock(attrs \\ %{}) do
    Map.merge(
      %{
        username: random_string(),
        password: "password:#{random_string()}",
        account_id: random_integer(),
        encryption_key: random_integer()
      },
      attrs
    )
  end
end
