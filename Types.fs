namespace YachtDice.Types

type Dice = int list

type Score = int

type ScoreCategory =
    | Ones
    | Twos
    | Threes
    | Fours
    | Fives
    | Sixes
    | Choice
    | FourOfAKind
    | FullHouse
    | SmallStraight
    | LargeStraight
    | High
    | Low
    | Evens
    | Odds
    | Zero
    | Yacht

type GameState = {
    Dice: Dice
    Scores: Map<ScoreCategory, Score option> //Score can be None if not yet scored, or Some int if scored.
    RollCount: int // Number of rolls in the current turn (1 to 3)
    TurnCount: int // Number of turns taken (0 to 17)
}