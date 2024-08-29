defmodule ElvenDatabase.Players.Character do
  @moduledoc """
  Holds information about a Character.
  """

  use Ecto.Schema

  import Ecto.Changeset
  # import EctoBitfield

  require ElvenData.Enums.PlayerEnums, as: PlayerEnums

  alias __MODULE__
  alias ElvenDatabase.Players.{Account, Item}

  @type id :: non_neg_integer()
  @type slot :: 0..4
  @type t :: %Character{
          id: id(),
          account_id: Account.id(),
          name: String.t(),
          slot: slot(),
          class: PlayerEnums.character_class_keys(),
          faction: PlayerEnums.faction_keys(),
          gender: PlayerEnums.gender_keys(),
          hair_color: PlayerEnums.hair_color_keys(),
          hair_style: PlayerEnums.hair_style_keys(),
          map_id: non_neg_integer(),
          map_x: non_neg_integer(),
          map_y: non_neg_integer(),
          additional_hp: integer(),
          additional_mp: integer(),
          gold: non_neg_integer(),
          bank_gold: non_neg_integer(),
          biography: String.t(),
          level: pos_integer(),
          job_level: pos_integer(),
          hero_level: non_neg_integer(),
          level_xp: non_neg_integer(),
          job_level_xp: non_neg_integer(),
          hero_level_xp: non_neg_integer(),
          sp_points: non_neg_integer(),
          sp_additional_points: non_neg_integer(),
          rage_points: non_neg_integer(),
          max_mate_count: non_neg_integer(),
          reputation: non_neg_integer(),
          dignity: non_neg_integer(),
          compliment: non_neg_integer(),
          act4_dead: non_neg_integer(),
          act4_kill: non_neg_integer(),
          act4_points: non_neg_integer(),
          arena_winner: boolean(),
          talent_win: non_neg_integer(),
          talent_lose: non_neg_integer(),
          talent_surrender: non_neg_integer(),
          master_points: non_neg_integer(),
          master_ticket: non_neg_integer(),
          miniland_intro: String.t(),
          miniland_state: PlayerEnums.miniland_state_keys(),
          miniland_makepoints: non_neg_integer(),
          items: [Item.t()],
          # Ecto fields
          __meta__: Ecto.Schema.Metadata.t(),
          inserted_at: any(),
          updated_at: any()
        }

  # defbitfield GameOptions,
  #   exchange_blocked: round(:math.pow(2, 1)),
  #   friend_request_blocked: round(:math.pow(2, 2)),
  #   family_request_blocked: round(:math.pow(2, 3)),
  #   whisper_blocked: round(:math.pow(2, 4)),
  #   group_request_blocked: round(:math.pow(2, 5)),
  #   mouse_aim_lock: round(:math.pow(2, 9)),
  #   global_chat_blocked: round(:math.pow(2, 10)),
  #   quick_get_up: round(:math.pow(2, 11)),
  #   emoticons_blocked: round(:math.pow(2, 12)),
  #   hp_stat_blocked: round(:math.pow(2, 13)),
  #   buff_counter_blocked: round(:math.pow(2, 14)),
  #   miniland_invite_blocked: round(:math.pow(2, 15)),
  #   hats_hidden: round(:math.pow(2, 16)),
  #   ui_locked: round(:math.pow(2, 17))

  ## Schema

  schema "visible_characters" do
    belongs_to :account, ElvenDatabase.Players.Account

    field :name, :string
    field :slot, :integer

    field :class, Ecto.Enum, values: PlayerEnums.character_class(:__keys__)
    field :faction, Ecto.Enum, values: PlayerEnums.faction(:__keys__)
    field :gender, Ecto.Enum, values: PlayerEnums.gender(:__keys__)
    field :hair_color, Ecto.Enum, values: PlayerEnums.hair_color(:__keys__)
    field :hair_style, Ecto.Enum, values: PlayerEnums.hair_style(:__keys__)

    field :map_id, :integer
    field :map_x, :integer
    field :map_y, :integer

    field :additional_hp, :integer
    field :additional_mp, :integer
    field :gold, :integer
    field :bank_gold, :integer
    field :biography, :string

    field :level, :integer
    field :job_level, :integer
    field :hero_level, :integer
    field :level_xp, :integer
    field :job_level_xp, :integer
    field :hero_level_xp, :integer

    field :sp_points, :integer
    field :sp_additional_points, :integer
    field :rage_points, :integer
    field :max_mate_count, :integer

    field :reputation, :integer
    field :dignity, :integer
    field :compliment, :integer

    field :act4_dead, :integer
    field :act4_kill, :integer
    field :act4_points, :integer
    field :arena_winner, :boolean
    field :talent_win, :integer
    field :talent_lose, :integer
    field :talent_surrender, :integer
    field :master_points, :integer
    field :master_ticket, :integer

    field :miniland_intro, :string
    field :miniland_state, Ecto.Enum, values: PlayerEnums.miniland_state(:__keys__)
    field :miniland_makepoints, :integer

    # field :game_options, GameOptions

    has_many :items, Item, foreign_key: :owner_id

    field :deleted_at, :utc_datetime
    timestamps()
  end

  @required_fields [
    :account_id,
    :name,
    :slot,
    :gender,
    :hair_color,
    :hair_style,
    :map_id,
    :map_x,
    :map_y
  ]

  @optional_fields [
    :class,
    :faction,
    :additional_hp,
    :additional_mp,
    :gold,
    :bank_gold,
    :biography,
    :level,
    :job_level,
    :hero_level,
    :level_xp,
    :job_level_xp,
    :hero_level_xp,
    :sp_points,
    :sp_additional_points,
    :rage_points,
    :max_mate_count,
    :reputation,
    :dignity,
    :compliment,
    :act4_dead,
    :act4_kill,
    :act4_points,
    :arena_winner,
    :talent_win,
    :talent_lose,
    :talent_surrender,
    :master_points,
    :master_ticket,
    :miniland_intro,
    :miniland_state,
    :miniland_makepoints
    # :game_options
  ]

  ## Public API

  @fields @required_fields ++ @optional_fields
  @name_regex ~r/^[\x21-\x7E\xA1-\xAC\xAE-\xFF\x{4e00}-\x{9fa5}\x{0E01}-\x{0E3A}\x{0E3F}-\x{0E5B}\x2E]+$/u

  @spec changeset(t(), map()) :: Ecto.Changeset.t()
  def changeset(%Character{} = character, attrs) do
    changeset(character, attrs, @required_fields)
  end

  @spec assoc_changeset(t(), map()) :: Ecto.Changeset.t()
  def assoc_changeset(%Character{} = character, attrs) do
    # In case of cast_assoc, :account_id field is automatically created so it's
    # not required
    changeset(character, attrs, List.delete(@required_fields, :account_id))
  end

  ## Private functions

  defp changeset(character, attrs, required_fields) do
    attrs =
      case attrs do
        %{account: %Account{} = account} -> Map.put(attrs, :account_id, account.id)
        attrs -> attrs
      end

    character
    |> cast(attrs, @fields)
    |> cast_assoc(:items, with: &Item.assoc_changeset/2)
    |> validate_required(required_fields)
    # TODO: Later support encoding for others languages like CH, JP, ...
    |> validate_length(:name, min: 4, max: 14)
    |> validate_format(:name, @name_regex)
    |> assoc_constraint(:account)
    |> unique_constraint(:name, name: :characters_name)
    |> unique_constraint(:slot, name: :account_slot)
  end
end
