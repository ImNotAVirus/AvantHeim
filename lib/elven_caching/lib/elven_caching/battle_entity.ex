defprotocol ElvenCaching.BattleEntity do
  @moduledoc """
  TODO: Documentation
  """

  ## FIXME: Move hp, mp, hp_max, mp_max into another protocol because
  ## NPC don't have to implement can_attack and can_move functions

  @type hp :: non_neg_integer()
  @type hp_max :: pos_integer()
  @type mp :: non_neg_integer()
  @type mp_max :: pos_integer()

  @doc "Get the BattleEntity hp"
  @spec hp(t()) :: hp()
  def hp(entity)

  @doc "Set the BattleEntity hp"
  @spec hp(t(), hp()) :: t()
  def hp(entity, hp)

  @doc "Get the BattleEntity hp_max"
  @spec hp_max(t()) :: hp_max()
  def hp_max(entity)

  @doc "Set the BattleEntity hp_max"
  @spec hp_max(t(), hp_max()) :: t()
  def hp_max(entity, hp_max)

  @doc "Get the BattleEntity mp"
  @spec mp(t()) :: mp()
  def mp(entity)

  @doc "Set the BattleEntity mp"
  @spec mp(t(), mp()) :: t()
  def mp(entity, mp)

  @doc "Get the BattleEntity mp_max"
  @spec mp_max(t()) :: mp_max()
  def mp_max(entity)

  @doc "Set the BattleEntity mp_max"
  @spec mp_max(t(), mp_max()) :: t()
  def mp_max(entity, mp_max)

  @doc "Get the BattleEntity can_attack"
  @spec can_attack(t()) :: boolean
  def can_attack(entity)

  @doc "Set the BattleEntity can_attack"
  @spec can_attack(t(), boolean) :: t()
  def can_attack(entity, can_attack)

  @doc "Get the BattleEntity can_move"
  @spec can_move(t()) :: boolean
  def can_move(entity)

  @doc "Set the BattleEntity can_move"
  @spec can_move(t(), boolean) :: t()
  def can_move(entity, can_move)
end
