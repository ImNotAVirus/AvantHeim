defmodule ElvenAlgorithms.PlayerAlgorithms do
  @moduledoc """
  TODO: Documentation
  """

  import ElvenAlgorithms

  @max_level 99

  ## HP Max Algorithm

  @hp_constant %{
    adventurer: 0,
    swordman: 8,
    archer: 3,
    magician: 0,
    martial_artist: 5
  }

  for level <- 1..@max_level, {class, multiplier} <- @hp_constant do
    hpx = level + floor((level - 1) * multiplier / 10)
    hp = trunc(0.5 * hpx ** 2 + 15.5 * hpx + 205)

    def hp_max(unquote(class), unquote(level)) do
      unquote(hp)
    end
  end

  ## MP Max Algorithm

  @mp_constant %{
    adventurer: 0,
    swordman: 0,
    archer: 1,
    magician: 8,
    martial_artist: 2
  }

  for level <- 1..@max_level, {class, multiplier} <- @mp_constant do
    mpx = level + floor((level - 1) * multiplier / 10)

    mp =
      floor(9.25 * mpx + 50.75) +
        trunc((mpx - 2) / 4) * 2 * (modulus(mpx - 2, 4) + 1 + trunc((mpx - 6) / 4) * 2)

    mp_trunc = trunc(mp)

    def mp_max(unquote(class), unquote(level)) do
      unquote(mp_trunc)
    end
  end

  ## Close Defense Algorithm

  @close_defense_constant %{
    adventurer:
      List.flatten([
        [5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12],
        [12, 13, 13, 14, 14, 15, 15, 16, 16, 17, 17, 18, 18],
        [19, 19, 20, 20, 21, 21, 22, 22, 23, 23, 24, 24],
        [25, 25, 26, 26, 27, 27, 28, 28, 29, 29, 30, 30, 31],
        [31, 32, 32, 33, 33, 34, 34, 35, 35, 36, 36, 37, 37],
        [38, 38, 39, 39, 40, 40, 41, 41, 42, 42, 43, 43, 44],
        [44, 45, 45, 46, 46, 47, 47, 48, 48, 49, 49, 50, 50],
        [51, 51, 52, 52, 53, 53, 54]
      ]),
    swordman:
      List.flatten([
        [5, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 14, 15, 16, 17],
        [18, 19, 20, 21, 22, 23, 23, 24, 25, 26, 27, 28, 29],
        [30, 31, 32, 32, 33, 34, 35, 36, 37, 38, 39, 40],
        [41, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 50, 51],
        [52, 53, 54, 55, 56, 57, 58, 59, 59, 60, 61, 62, 63],
        [64, 65, 66, 67, 68, 68, 69, 70, 71, 72, 73, 74, 75],
        [76, 77, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 86],
        [87, 88, 89, 90, 91, 92, 93]
      ]),
    archer:
      List.flatten([
        [5, 5, 6, 6, 7, 8, 8, 9, 10, 10, 11, 12, 12, 13, 14],
        [14, 15, 16, 16, 17, 18, 18, 19, 19, 20, 21, 21],
        [22, 23, 23, 24, 25, 25, 26, 27, 27, 28, 29, 29, 30],
        [31, 31, 32, 32, 33, 34, 34, 35, 36, 36, 37, 38, 38],
        [39, 40, 40, 41, 42, 42, 43, 44, 44, 45, 45, 46, 47],
        [47, 48, 49, 49, 50, 51, 51, 52, 53, 53, 54, 55, 55],
        [56, 57, 57, 58, 58, 59, 60, 60, 61, 62, 62, 63, 64],
        [64, 65, 66, 66, 67, 68, 68]
      ]),
    magician:
      List.flatten([
        [5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12],
        [12, 13, 13, 14, 14, 15, 15, 16, 16, 17, 17, 18, 18],
        [19, 19, 20, 20, 21, 21, 22, 22, 23, 23, 24, 24, 25],
        [25, 26, 26, 27, 27, 28, 28, 29, 29, 30, 30, 31, 31],
        [32, 32, 33, 33, 34, 34, 35, 35, 36, 36, 37, 37, 38],
        [38, 39, 39, 40, 40, 41, 41, 42, 42, 43, 43, 44, 44],
        [45, 45, 46, 46, 47, 47, 48, 48, 49, 49, 50, 50, 51],
        [51, 52, 52, 53, 53, 54]
      ]),
    martial_artist:
      List.flatten([
        [5, 5, 6, 7, 8, 8, 9, 10, 11, 11, 12, 13, 14, 14, 15],
        [16, 17, 17, 18, 19, 20, 20, 21, 22, 23, 23, 24, 25],
        [26, 26, 27, 28, 29, 29, 30, 31, 32, 32, 33, 34, 35],
        [35, 36, 37, 38, 38, 39, 40, 41, 41, 42, 43, 44, 44],
        [45, 46, 47, 47, 48, 49, 50, 50, 51, 52, 53, 53, 54],
        [55, 56, 56, 57, 58, 59, 59, 60, 61, 62, 62, 63, 64],
        [65, 65, 66, 67, 68, 68, 69, 70, 71, 71, 72, 73, 74],
        [74, 75, 76, 77, 77, 78]
      ])
  }

  for {class, constants} <- @close_defense_constant,
      {constant, level} <- Enum.with_index(constants, 1) do
    def close_defense(unquote(class), unquote(level)) do
      unquote(constant)
    end
  end

  ## Distance Defense Algorithm

  @distance_defense_constant %{
    adventurer:
      List.flatten([
        [5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12],
        [12, 13, 13, 14, 14, 15, 15, 16, 16, 17, 17, 18, 18],
        [19, 19, 20, 20, 21, 21, 22, 22, 23, 23, 24, 24, 25],
        [25, 26, 26, 27, 27, 28, 28, 29, 29, 30, 30, 31, 31],
        [32, 32, 33, 33, 34, 34, 35, 35, 36, 36, 37, 37, 38],
        [38, 39, 39, 40, 40, 41, 41, 42, 42, 43, 43, 44, 44],
        [45, 45, 46, 46, 47, 47, 48, 48, 49, 49, 50, 50, 51],
        [51, 52, 52, 53, 53, 54]
      ]),
    swordman:
      List.flatten([
        [5, 5, 6, 6, 7, 8, 8, 9, 9, 10, 11, 11, 12, 12, 13],
        [14, 14, 15, 15, 16, 17, 17, 18, 18, 19, 20, 20, 21],
        [21, 22, 23, 23, 24, 24, 25, 26, 26, 27, 27, 28, 29],
        [29, 30, 30, 31, 32, 32, 33, 33, 34, 35, 35, 36, 36],
        [37, 38, 38, 39, 39, 40, 41, 41, 42, 42, 43, 44, 44],
        [45, 45, 46, 47, 47, 48, 48, 49, 50, 50, 51, 51, 52],
        [53, 53, 54, 54, 55, 56, 56, 57, 57, 58, 59, 59, 60],
        [60, 61, 62, 62, 63, 63]
      ]),
    archer:
      List.flatten([
        [5, 5, 6, 7, 8, 9, 9, 10, 11, 12, 13, 13, 14, 15, 16],
        [17, 17, 18, 19, 20, 21, 21, 22, 23, 24, 25, 25, 26],
        [27, 28, 29, 29, 30, 31, 32, 33, 33, 34, 35, 36, 37],
        [37, 38, 39, 40, 41, 41, 42, 43, 44, 45, 45, 46, 47],
        [48, 49, 49, 50, 51, 52, 53, 53, 54, 55, 56, 57, 57],
        [58, 59, 60, 61, 61, 62, 63, 64, 65, 65, 66, 67, 68],
        [69, 69, 70, 71, 72, 73, 73, 74, 75, 76, 77, 77, 78],
        [79, 80, 81, 81, 82, 83]
      ]),
    magician:
      List.flatten([
        [25, 25, 26, 26, 27, 28, 28, 29, 29, 30, 31, 31, 32],
        [32, 33, 34, 34, 35, 35, 36, 37, 37, 38, 38, 39, 40],
        [40, 41, 41, 42, 43, 43, 44, 44, 45, 46, 46, 47, 47],
        [48, 49, 49, 50, 50, 51, 52, 52, 53, 53, 54, 55, 55],
        [56, 56, 57, 58, 58, 59, 59, 60, 61, 61, 62, 62, 63],
        [64, 64, 65, 65, 66, 67, 67, 68, 68, 69, 70, 70, 71],
        [71, 72, 73, 73, 74, 74, 75, 76, 76, 77, 77, 78, 79],
        [79, 80, 80, 81, 82, 82, 83, 83]
      ]),
    martial_artist:
      List.flatten([
        [15, 15, 16, 16, 17, 18, 18, 19, 20, 20, 21, 22, 22],
        [23, 24, 24, 25, 26, 26, 27, 28, 28, 29, 29, 30, 31],
        [31, 32, 33, 33, 34, 35, 35, 36, 37, 37, 38, 39, 39],
        [40, 41, 41, 42, 42, 43, 44, 44, 45, 46, 46, 47, 48],
        [48, 49, 50, 50, 51, 52, 52, 53, 54, 54, 55, 55, 56],
        [57, 57, 58, 59, 59, 60, 61, 61, 62, 63, 63, 64, 65],
        [65, 66, 67, 67, 68, 68, 69, 70, 70, 71, 72, 72, 73],
        [74, 74, 75, 76, 76, 77, 78, 78]
      ])
  }

  for {class, constants} <- @distance_defense_constant,
      {constant, level} <- Enum.with_index(constants, 1) do
    def distance_defense(unquote(class), unquote(level)) do
      unquote(constant)
    end
  end

  ## Magic Defense Algorithm

  @magic_defense_constant %{
    adventurer:
      List.flatten([
        [5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12],
        [13, 13, 14, 14, 15, 15, 16, 16, 17, 17, 18, 18, 19],
        [19, 20, 20, 21, 21, 22, 22, 23, 23, 24, 24, 25, 25],
        [26, 26, 27, 27, 28, 28, 29, 29, 30, 30, 31, 31, 32],
        [32, 33, 33, 34, 34, 35, 35, 36, 36, 37, 37, 38, 38],
        [39, 39, 40, 40, 41, 41, 42, 42, 43, 43, 44, 44, 45],
        [45, 46, 46, 47, 47, 48, 48, 49, 49, 50, 50, 51, 51],
        [52, 52, 53, 53, 54]
      ]),
    swordman:
      List.flatten([
        [5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12],
        [13, 13, 14, 14, 15, 15, 16, 16, 17, 17, 18, 18, 19],
        [19, 20, 20, 21, 21, 22, 22, 23, 23, 24, 24, 25, 25],
        [26, 26, 27, 27, 28, 28, 29, 29, 30, 30, 31, 31, 32],
        [32, 33, 33, 34, 34, 35, 35, 36, 36, 37, 37, 38, 38],
        [39, 39, 40, 40, 41, 41, 42, 42, 43, 43, 44, 44, 45],
        [45, 46, 46, 47, 47, 48, 48, 49, 49, 50, 50, 51, 51],
        [52, 52, 53, 53, 54]
      ]),
    archer:
      List.flatten([
        [5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 11, 11, 12, 12, 13],
        [13, 14, 14, 15, 16, 16, 17, 17, 18, 18, 19, 19, 20],
        [20, 21, 22, 22, 23, 23, 24, 24, 25, 25, 26, 27, 27],
        [28, 28, 29, 29, 30, 30, 31, 31, 32, 33, 33, 34, 34],
        [35, 35, 36, 36, 37, 38, 38, 39, 39, 40, 40, 41, 41],
        [42, 42, 43, 44, 44, 45, 45, 46, 46, 47, 47, 48, 49],
        [49, 50, 50, 51, 51, 52, 52, 53, 53, 54, 55, 55, 56],
        [56, 57, 57, 58, 58]
      ]),
    magician:
      List.flatten([
        [5, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 14, 15, 16, 17],
        [18, 19, 20, 21, 22, 23, 23, 24, 25, 26, 27, 28, 29],
        [30, 31, 32, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41],
        [41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 50, 51, 52],
        [53, 54, 55, 56, 57, 58, 59, 59, 60, 61, 62, 63, 64],
        [65, 66, 67, 68, 68, 69, 70, 71, 72, 73, 74, 75, 76],
        [77, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 86, 87],
        [88, 89, 90, 91, 92, 93]
      ]),
    martial_artist:
      List.flatten([
        [5, 5, 6, 6, 7, 8, 8, 9, 9, 10, 11, 11, 12, 12, 13, 14],
        [14, 15, 15, 16, 17, 17, 18, 18, 19, 20, 20, 21, 21],
        [22, 23, 23, 24, 24, 25, 26, 26, 27, 27, 28, 29, 29],
        [30, 30, 31, 32, 32, 33, 33, 34, 35, 35, 36, 36, 37],
        [38, 38, 39, 39, 40, 41, 41, 42, 42, 43, 44, 44, 45],
        [45, 46, 47, 47, 48, 48, 49, 50, 50, 51, 51, 52, 53],
        [53, 54, 54, 55, 56, 56, 57, 57, 58, 59, 59, 60, 60],
        [61, 62, 62, 63, 63]
      ])
  }

  for {class, constants} <- @magic_defense_constant,
      {constant, level} <- Enum.with_index(constants, 1) do
    def magic_defense(unquote(class), unquote(level)) do
      unquote(constant)
    end
  end

  ## Hit Dodge Algorithm

  @hit_dodge_constant %{
    adventurer:
      List.flatten([
        [10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23],
        [24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37],
        [38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51],
        [52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65],
        [66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79],
        [80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93],
        [94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105],
        [106, 107, 108]
      ]),
    swordman:
      List.flatten([
        [10, 11, 12, 13, 14, 16, 17, 18, 19, 20, 22, 23, 24, 25],
        [26, 28, 29, 30, 31, 32, 34, 35, 36, 37, 38, 40, 41, 42],
        [43, 44, 46, 47, 48, 49, 50, 52, 53, 54, 55, 56, 58, 59],
        [60, 61, 62, 64, 65, 66, 67, 68, 70, 71, 72, 73, 74, 76],
        [77, 78, 79, 80, 82, 83, 84, 85, 86, 88, 89, 90, 91, 92],
        [94, 95, 96, 97, 98, 100, 101, 102, 103, 104, 106, 107],
        [108, 109, 110, 112, 113, 114, 115, 116, 118, 119, 120],
        [121, 122, 124, 125, 126, 127]
      ]),
    archer:
      List.flatten([
        [20, 21, 23, 24, 26, 28, 29, 31, 32, 34, 36, 37, 39, 40],
        [42, 44, 45, 47, 48, 50, 52, 53, 55, 56, 58, 60, 61, 63],
        [64, 66, 68, 69, 71, 72, 74, 76, 77, 79, 80, 82, 84, 85],
        [87, 88, 90, 92, 93, 95, 96, 98, 100, 101, 103, 104, 106],
        [108, 109, 111, 112, 114, 116, 117, 119, 120, 122, 124],
        [125, 127, 128, 130, 132, 133, 135, 136, 138, 140, 141],
        [143, 144, 146, 148, 149, 151, 152, 154, 156, 157, 159],
        [160, 162, 164, 165, 167, 168, 170, 172, 173, 175, 176]
      ]),
    magician:
      List.flatten([
        [20, 21, 22, 23, 24, 26, 27, 28, 29, 30, 32, 33, 34, 35],
        [36, 38, 39, 40, 41, 42, 44, 45, 46, 47, 48, 50, 51, 52],
        [53, 54, 56, 57, 58, 59, 60, 62, 63, 64, 65, 66, 68, 69],
        [70, 71, 72, 74, 75, 76, 77, 78, 80, 81, 82, 83, 84, 86],
        [87, 88, 89, 90, 92, 93, 94, 95, 96, 98, 99, 100, 101, 102],
        [104, 105, 106, 107, 108, 110, 111, 112, 113, 114, 116, 117],
        [118, 119, 120, 122, 123, 124, 125, 126, 128, 129, 130, 131],
        [132, 134, 135, 136, 137]
      ]),
    martial_artist:
      List.flatten([
        [30, 31, 32, 33, 35, 36, 37, 39, 40, 41, 43, 44, 45, 46],
        [48, 49, 50, 52, 53, 54, 56, 57, 58, 59, 61, 62, 63, 65],
        [66, 67, 69, 70, 71, 72, 74, 75, 76, 78, 79, 80, 82, 83],
        [84, 85, 87, 88, 89, 91, 92, 93, 95, 96, 97, 98, 100, 101],
        [102, 104, 105, 106, 108, 109, 110, 111, 113, 114, 115, 117],
        [118, 119, 121, 122, 123, 124, 126, 127, 128, 130, 131, 132],
        [134, 135, 136, 137, 139, 140, 141, 143, 144, 145, 147, 148],
        [149, 150, 152, 153, 154, 156, 157]
      ])
  }

  for {class, constants} <- @hit_dodge_constant,
      {constant, level} <- Enum.with_index(constants, 1) do
    def hit_dodge(unquote(class), unquote(level)) do
      unquote(constant)
    end
  end

  ## Distance Dodge Algorithm

  @distance_dodge_constant %{
    adventurer:
      List.flatten([
        [10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23],
        [24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37],
        [38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51],
        [52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65],
        [66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79],
        [80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93],
        [94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105],
        [106, 107, 108]
      ]),
    swordman:
      List.flatten([
        [10, 11, 12, 13, 14, 16, 17, 18, 19, 20, 22, 23, 24, 25],
        [26, 28, 29, 30, 31, 32, 34, 35, 36, 37, 38, 40, 41, 42],
        [43, 44, 46, 47, 48, 49, 50, 52, 53, 54, 55, 56, 58, 59],
        [60, 61, 62, 64, 65, 66, 67, 68, 70, 71, 72, 73, 74, 76],
        [77, 78, 79, 80, 82, 83, 84, 85, 86, 88, 89, 90, 91, 92],
        [94, 95, 96, 97, 98, 100, 101, 102, 103, 104, 106, 107],
        [108, 109, 110, 112, 113, 114, 115, 116, 118, 119, 120],
        [121, 122, 124, 125, 126, 127]
      ]),
    archer:
      List.flatten([
        [20, 21, 23, 24, 26, 28, 29, 31, 32, 34, 36, 37, 39, 40],
        [42, 44, 45, 47, 48, 50, 52, 53, 55, 56, 58, 60, 61, 63],
        [64, 66, 68, 69, 71, 72, 74, 76, 77, 79, 80, 82, 84, 85],
        [87, 88, 90, 92, 93, 95, 96, 98, 100, 101, 103, 104, 106],
        [108, 109, 111, 112, 114, 116, 117, 119, 120, 122, 124],
        [125, 127, 128, 130, 132, 133, 135, 136, 138, 140, 141],
        [143, 144, 146, 148, 149, 151, 152, 154, 156, 157, 159],
        [160, 162, 164, 165, 167, 168, 170, 172, 173, 175, 176]
      ]),
    magician:
      List.flatten([
        [10, 11, 12, 13, 14, 16, 17, 18, 19, 20, 22, 23, 24, 25],
        [26, 28, 29, 30, 31, 32, 34, 35, 36, 37, 38, 40, 41, 42],
        [43, 44, 46, 47, 48, 49, 50, 52, 53, 54, 55, 56, 58, 59],
        [60, 61, 62, 64, 65, 66, 67, 68, 70, 71, 72, 73, 74, 76],
        [77, 78, 79, 80, 82, 83, 84, 85, 86, 88, 89, 90, 91, 92],
        [94, 95, 96, 97, 98, 100, 101, 102, 103, 104, 106, 107],
        [108, 109, 110, 112, 113, 114, 115, 116, 118, 119, 120],
        [121, 122, 124, 125, 126, 127]
      ]),
    martial_artist:
      List.flatten([
        [20, 21, 22, 23, 25, 26, 27, 29, 30, 31, 33, 34, 35, 36],
        [38, 39, 40, 42, 43, 44, 46, 47, 48, 49, 51, 52, 53, 55],
        [56, 57, 59, 60, 61, 62, 64, 65, 66, 68, 69, 70, 72, 73],
        [74, 75, 77, 78, 79, 81, 82, 83, 85, 86, 87, 88, 90, 91],
        [92, 94, 95, 96, 98, 99, 100, 101, 103, 104, 105, 107],
        [108, 109, 111, 112, 113, 114, 116, 117, 118, 120, 121],
        [122, 124, 125, 126, 127, 129, 130, 131, 133, 134, 135],
        [137, 138, 139, 140, 142, 143, 144, 146, 147]
      ])
  }

  for {class, constants} <- @distance_dodge_constant,
      {constant, level} <- Enum.with_index(constants, 1) do
    def distance_dodge(unquote(class), unquote(level)) do
      unquote(constant)
    end
  end
end
