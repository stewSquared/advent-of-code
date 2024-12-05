# Advent of Code 2023

The solutions in this folder are all written during live streams on and cleaned up afterwards.

Questions or comments about my solutions? Drop by my [Discord](https://discord.gg/SgZemzbHPa) to chat about it.

Wanna see these in the making? Visit my channel on [Twitch](https://twitch.tv/stewSquared) or [YouTube](https://youtube.com/@stewSquared) just before the problems come out at midnight EST. I start a little early for review/refactor/warmup.

A [playlist](https://www.youtube.com/playlist?list=PLnP_dObOt-rWnSqOUQDc5T_r9TMdaNxJl) of live recordings is available. Recordings typically include some warmup, refactor, recap, and development of library code. Directl links in the table below will link to the timestamp of the actual solution.

As problems come out, I'll fill in this table of stats and links to recordings below.

## Solution Stats and Details

| Day | P1 Time  | P1 Rank | P2 Time  | P2 Rank | LoC | Recording Links | Notes |
| --: | -------: | ------: | -------: | ------: | --: | --------------- | ----- |
|  01 | 00:04:41 |    1836 | 00:18:44 |    1557 | | [Part 1](https://www.youtube.com/watch?v=Jv9B1crzpWM) [Part 2](https://www.youtube.com/watch?v=Jv9B1crzpWM&t=4m38s) | |
|  02 | 00:10:30 |    1405 | 00:13:18 |    1169 | | [Part 1]() [Part 2]() | |
|  03 | 00:36:42 |    4078 | 00:45:47 |    3022 | | [Part 1]() [Part 2]() | |
|  04 | 00:12:58 |    4625 | 00:43:37 |    6925 | | [Part 1]() [Part 2]() | |
|  05 | 01:27:36 |   10669 | 02:24:39 |    5307 | | [Part 1]() [Part 2]() | |
|  06 | 00:14:42 |    4901 | 00:32:47 |    7766 | | [Part 1]() [Part 2]() | |
|  07 | 01:45:56 |   10791 | 02:13:01 |    9090 | | [Part 1]() [Part 2]() | |
|  08 | 00:12:00 |    3081 | 01:53:13 |    8419 | | [Part 1]() [Part 2]() | |
|  09 | 00:17:04 |    3181 | 00:18:17 |    2203 | | [Part 1]() [Part 2]() | |
|  10 | 00:37:45 |    2084 | 02:26:46 |    3264 | | [Part 1]() [Part 2]() | |
|  11 | 00:28:59 |    3656 | 00:31:42 |    2352 | | [Part 1]() [Part 2]() | |
|  12 | 00:46:33 |    3326 |      N/A |     N/A | | [Part 1]() [Part 2]() | Stopped early and finished during the next night's warmup |
|  13 | 01:08:00 |    5076 | 01:11:08 |    3273 | | [Part 1]() [Part 2]() | |
|  14 | 00:33:57 |    5042 |      N/A |     N/A | | [Part 1]() [Part 2]() | Stopped early and finished during the next night's warmup |
|  15 | 00:03:55 |     702 | 00:33:21 |    2583 | | [Part 1]() [Part 2]() | |
|  16 | 00:29:22 |    1525 | 00:35:52 |    1356 | | [Part 1]() [Part 2]() | |

## Solution Code Highlights

<!-- <details>
  <summary>Day 02</summary>

```scala
case class Game(id: Int, hands: List[Map[String, Int]])

val possibleGames = games
  .filter(_.hands.map(_("red")).max <= 12)
  .filter(_.hands.map(_("green")).max <= 13)
  .filter(_.hands.map(_("blue")).max <= 14)

def power(game: Game): Int =
  game.hands.map(_("red")).max * game.hands.map(_("green")).max * game.hands.map(_("blue")).max

val ans = possibleGames.map(_.id).sum
val ans2 = games.map(power).sum
```

</details> -->
