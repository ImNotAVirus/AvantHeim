defmodule LoginService.SessionManager.Session do
  @moduledoc """
  TODO: Documentation for LoginService.SessionManager.Session
  """

  alias __MODULE__

  @enforce_keys [:account_id, :username, :password, :encryption_key]
  defstruct [:account_id, :username, :password, :encryption_key, :monitor]

  @type t :: %Session{
          username: String.t(),
          password: String.t(),
          account_id: pos_integer(),
          encryption_key: 0..65535,
          monitor: nil | pid
        }

  ## Public API

  @spec new(map()) :: t()
  def new(attrs), do: struct!(Session, attrs)
end
