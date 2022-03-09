defmodule ElvenAlgorithms.EntityAlgorithmsTest do
  use ExUnit.Case

  alias ElvenAlgorithms.EntityAlgorithms

  ## Tests

  describe "speed/1 for" do
    test "adventurer" do
      assert EntityAlgorithms.speed(:adventurer) == 11
    end

    test "swordman" do
      assert EntityAlgorithms.speed(:swordman) == 11
    end

    test "archer" do
      assert EntityAlgorithms.speed(:archer) == 12
    end

    test "magician" do
      assert EntityAlgorithms.speed(:magician) == 10
    end

    test "martial_artist" do
      assert EntityAlgorithms.speed(:martial_artist) == 11
    end
  end
end
