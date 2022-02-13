defmodule EnumCase do
  use ExUnit.CaseTemplate

  setup do
    # This will run before each test that uses this case
    :ok
  end

  using do
    quote do
      import EnumCase, only: [test_enum: 2]
    end
  end

  ## Helpers

  defmacro test_enum(module, name) do
    exp_mod = Macro.expand(module, __CALLER__)

    quote location: :keep do
      test "Enum #{inspect(unquote(exp_mod))}.#{unquote(name)} is defined" do
        assert [_ | _] = unquote(exp_mod).unquote(name)(:__enumerators__)
      end
    end
  end
end
