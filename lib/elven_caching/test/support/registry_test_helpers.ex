defmodule RegistryTestHelpers do
  def sync(process) do
    # Some functions like send/2 or handle_continue
    # are async so you need a way to sync the proccess
    # :sys.get_state/1 is a easy workaround
    :sys.get_state(process)
  end
end
