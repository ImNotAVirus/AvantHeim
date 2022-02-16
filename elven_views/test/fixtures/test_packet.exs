defmodule MyApp.TestPacketEnums do
  import SimpleEnum, only: [defenum: 2]

  defenum :type, ~w(user admin)a
end

defmodule MyApp.SerializableTestStruct do
  defstruct key1: 123, key2: 456

  alias __MODULE__

  @type t :: %SerializableTestStruct{key1: integer, key2: integer}

  defimpl ElvenCore.Socket.SerializerProtocol do
    def serialize(%SerializableTestStruct{key1: key1, key2: key2}, _) do
      "#{key1}-#{key2}"
    end
  end
end

defmodule MyApp.TestPacket do
  use ElvenViews.SerializablePacket

  require MyApp.TestPacketEnums

  alias MyApp.SerializableTestStruct
  alias MyApp.TestPacketEnums

  defpacket "test" do
    field :id, :pos_integer
    field :id2, :non_neg_integer, default: 0
    field :id3, :integer, default: -123
    field :type, :enum, values: TestPacketEnums.type(:__enumerators__)
    field :message, :string
    field :my_struct, SerializableTestStruct
  end
end
