defmodule PresenceService do
  @moduledoc """
  Documentation for `PresenceService`.
  """

  defdelegate track(pid, session), to: PresenceService.Registry
  defdelegate get_session_by_id(account_id), to: PresenceService.Registry
  defdelegate get_session_by_name(account_name), to: PresenceService.Registry
end
