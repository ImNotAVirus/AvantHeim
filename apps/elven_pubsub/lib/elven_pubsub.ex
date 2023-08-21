defmodule ElvenPubSub do
  @moduledoc """
  Documentation for `ElvenPubSub`.
  """

  ## Public API

  def name(), do: :elven_pubsub

  def subscribe(topic, opts \\ []) do
    Phoenix.PubSub.subscribe(name(), topic, opts)
  end

  def broadcast(topic, message) do
    Phoenix.PubSub.broadcast(name(), topic, message)
  end
end
