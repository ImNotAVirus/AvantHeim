Code.require_file("../fixtures/test_packet.exs", __DIR__)

defmodule ElvenViews.SerializablePacketTest do
  use ExUnit.Case, async: true

  import ExUnit.CaptureIO
  # import ElvenCore.Socket.Serializer, only: [serialize_term: 2]

  # alias ElvenViews.SerializablePacket

  alias MyApp.TestPacket

  ## Tests

  describe "defpacket/2" do
    test "define a structure" do
      assert function_exported?(TestPacket, :__struct__, 1)
    end

    test "define typespec for the struture" do
      code = """
      defmodule MyApp.TestPacket2 do
        use ElvenViews.SerializablePacket

        require MyApp.TestPacketEnums
        
        alias MyApp.SerializableTestStruct
        alias MyApp.TestPacketEnums
        
        # Little trick to get module bytecode
        # I don't know if there is a better way to do that
        # Maybe use Compiler Tracing ?
        @after_compile __MODULE__
        
        defpacket "test" do
          field :id, :pos_integer
          field :id2, :non_neg_integer, default: 0
          field :id3, :integer, default: -123
          field :message, :string
          field :type, :enum, values: TestPacketEnums.type(:__enumerators__)
          field :my_struct, SerializableTestStruct
        end
        
        ## Print erlang module code

        def __after_compile__(_env, bytecode) do
          {:ok, abstract_code} = typespecs_abstract_code(bytecode)
          :io.fwrite('~s~n', [:erl_prettypr.format(:erl_syntax.form_list(abstract_code))])
        end
        
        # From https://github.com/elixir-lang/elixir/blob/main/lib/elixir/lib/code/typespec.ex#L156
        defp typespecs_abstract_code(binary) do
          with {:ok, {_, [debug_info: {:debug_info_v1, _backend, data}]}} <-
                 :beam_lib.chunks(binary, [:debug_info]),
               {:elixir_v1, %{}, specs} <- data do
            {:ok, specs}
          else
            _ -> :error
          end
        end
      end
      """

      log = capture_io(fn -> Code.compile_string(code) end)

      assert log =~ "-export_type([t/0])"
      assert log =~ "-type t() :: \#{'__struct__' :="
      assert log =~ "id := pos_integer"
      assert log =~ "id2 := non_neg_integer"
      assert log =~ "id3 := integer"
      assert log =~ "message := string"
      assert log =~ "type := 'Elixir.ElvenViews.SerializableEnum':t()}"
      assert log =~ "my_struct := 'Elixir.MyApp.SerializableTestStruct':t()"
    end
  end

  describe "new!/2" do
    test "returns a structure" do
      mock = test_packet_mock_attrs()
      packet = TestPacket.new!(mock)

      assert %TestPacket{} = packet
      assert packet.id == mock.id
      assert packet.id2 == 0
      assert packet.id3 == -123
      assert packet.type == mock.type
      assert packet.message == mock.message
      assert packet.my_struct == mock.my_struct
    end

    test "raises when required attr is missing" do
      assert_raise ArgumentError, ~r/no value provided for required field/, fn ->
        test_packet_mock_attrs() |> Map.delete(:id) |> TestPacket.new!()
      end
    end

    test "raises when invalid attr type" do
      assert_raise ArgumentError, "invalid types for [:id]", fn ->
        test_packet_mock_attrs() |> Map.put(:id, 0) |> TestPacket.new!()
      end

      assert_raise ArgumentError, "invalid types for [:id2]", fn ->
        test_packet_mock_attrs() |> Map.put(:id2, -1) |> TestPacket.new!()
      end

      assert_raise ArgumentError, "invalid types for [:id3]", fn ->
        test_packet_mock_attrs() |> Map.put(:id3, "test") |> TestPacket.new!()
      end

      assert_raise ArgumentError, "invalid types for [:type]", fn ->
        test_packet_mock_attrs() |> Map.put(:type, :test) |> TestPacket.new!()
      end

      assert_raise ArgumentError, "invalid types for [:message]", fn ->
        test_packet_mock_attrs() |> Map.put(:message, :foo) |> TestPacket.new!()
      end

      assert_raise ArgumentError, "invalid types for [:my_struct]", fn ->
        test_packet_mock_attrs() |> Map.put(:my_struct, MapSet.new()) |> TestPacket.new!()
      end
    end
  end

  describe "__using__/1" do
    test "define behaviour ElvenCore.SerializableStruct" do
      # serialize_term()
    end
  end

  ## Private helpers

  defp test_packet_mock_attrs() do
    %{
      id: 123,
      type: :admin,
      message: "This is a message",
      my_struct: %MyApp.SerializableTestStruct{key2: 789}
    }
  end
end
