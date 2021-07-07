defmodule CachingServiceTest do
  use ExUnit.Case
  doctest CachingService

  test "greets the world" do
    assert CachingService.hello() == :world
  end
end
