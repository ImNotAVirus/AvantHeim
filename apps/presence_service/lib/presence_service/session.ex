defmodule PresenceService.Session do
  @moduledoc """
  TODO: PresenceService.Session
  """

  alias __MODULE__

  @enforce_keys [:account_id, :username, :password, :encryption_key]
  defstruct [:account_id, :username, :password, :encryption_key]

  @type t :: %Session{
          account_id: pos_integer(),
          username: String.t(),
          password: String.t(),
          encryption_key: non_neg_integer()
        }

  ## Public API

  @spec new(map()) :: t()
  def new(attrs), do: struct!(Session, attrs)
end
