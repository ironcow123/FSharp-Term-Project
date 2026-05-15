module YachtDice.GameLogic

open YachtDice.Types
open YachtDice.Helper

let random = System.Random()

let rollDice() : Dice =
    List.init 5 (fun _ -> random.Next(1, 7))

let rollDiceNum(numDice: int): Dice =
    List.init numDice (fun _ -> random.Next(1, 7))

let isValidReroll (currentDice: int list) (inputIndices: int list) : bool =
    let uniqueInputs = inputIndices |> List.distinct
    uniqueInputs |> List.forall (fun x ->
        let inputCount = inputIndices |> List.filter (fun y -> y = x) |> List.length
        let diceCount = currentDice |> List.filter (fun y -> y = x) |> List.length
        inputCount <= diceCount
    )

let rerollDice (currentDice: Dice) (keepIndices: int list) : Dice =
    if isValidReroll currentDice keepIndices then (ListSubtract currentDice keepIndices) @ (rollDiceNum (List.length keepIndices))
    else currentDice //should never happen. just for safety.

let initGameState() : GameState =
    {
        Dice = rollDice()
        Scores = [ (Ones, None); (Twos, None); (Threes, None); (Fours, None); (Fives, None); (Sixes, None); (Choice, None); (FourOfAKind, None); (FullHouse, None); (SmallStraight, None); (LargeStraight, None); (High, None); (Low, None); (Evens, None); (Odds, None); (Zero, None); (Yacht, None) ] |> Map.ofList
        RollCount = 2
        TurnCount = 1
    }

let newGameState (newDice: Dice) (score: Map<ScoreCategory,Score option>) (RollCount: int) (TurnCount: int) : GameState =
    {    
        Dice = newDice
        Scores = score
        RollCount = RollCount
        TurnCount = TurnCount
    }

let isValidCategory (gameState: GameState) (category: ScoreCategory) : bool =
    match gameState.Scores |> Map.tryFind category with
    | Some (Some _) -> false // Category already scored
    | Some None -> true // Category is available but not scored
    | None -> true // means that the category is not in the map, invalid category, just for safety, should never happen.

let isGameOver (gameState: GameState) : bool =
    gameState.TurnCount >= 18

let calculateTotalScore (scores: Map<ScoreCategory, Score option>) : Score =
    scores |> Map.fold (fun acc _ scoreOption -> acc + (Option.defaultValue 0 scoreOption)) 0

let calculateBonusScore (scores: Map<ScoreCategory, Score option>) : Score =
    let upperSectionScore = [Ones; Twos; Threes; Fours; Fives; Sixes] |> List.sumBy (fun category -> scores |> Map.tryFind category |> Option.flatten |> Option.defaultValue 0)
    if upperSectionScore >= 63 then 35 else 0

let calculateFinalScore (scores: Map<ScoreCategory, Score option>) : Score =
    let totalScore = calculateTotalScore scores
    let bonusScore = calculateBonusScore scores
    totalScore + bonusScore
