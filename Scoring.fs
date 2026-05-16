module YachtDice.Scoring

open YachtDice.Types

let scoreOnes (dice: Dice) : Score = //input as list of ints(Dice), output as int(Score)
    dice |> List.filter (fun x -> x = 1) |> List.sum
let scoreTwos (dice: Dice) : Score =
    dice |> List.filter (fun x -> x = 2) |> List.sum
let scoreThrees (dice: Dice) : Score =
    dice |> List.filter (fun x -> x = 3) |> List.sum
let scoreFours (dice: Dice) : Score =
    dice |> List.filter (fun x -> x = 4) |> List.sum
let scoreFives (dice: Dice) : Score =
    dice |> List.filter (fun x -> x = 5) |> List.sum
let scoreSixes (dice: Dice) : Score =
    dice |> List.filter (fun x -> x = 6) |> List.sum

let scoreChoice (dice: Dice) : Score = List.sum dice
let scoreFourOfAKind (dice: Dice) : Score =
    if dice |> List.countBy id |> List.exists (fun (value, count) -> count >= 4) then List.sum dice else 0
let scoreFullHouse (dice: Dice) : Score =
    if (dice |> List.countBy id |> List.exists (fun (value, count) -> count = 3) &&
       dice |> List.countBy id |> List.exists (fun (value, count) -> count = 2)) || 
       (dice |> List.countBy id |> List.exists (fun (value, count) -> count = 5)) then List.sum dice else 0
//let scoreSmallStraight (dice: Dice) : Score =
//    let sortedDice =dice |> List.sort |> List.distinct
//    if (sortedDice = [1; 2; 3; 4]) || (sortedDice = [2; 3; 4; 5]) || (sortedDice = [3; 4; 5; 6]) then 15 else 0
let scoreSmallStraight (dice: Dice) : Score =
    let sortedDice = dice |> List.sort |> List.distinct
    let containsSubList (target: int list) = 
        target |> List.forall (fun x -> List.contains x sortedDice)        
    if containsSubList [1; 2; 3; 4] || 
       containsSubList [2; 3; 4; 5] || 
       containsSubList [3; 4; 5; 6] then 15 
    else 0

let scoreLargeStraight (dice: Dice) : Score =
    let sortedDice =dice |> List.sort |> List.distinct
    if sortedDice = [1; 2; 3; 4; 5] || sortedDice = [2; 3; 4; 5; 6] then 30 else 0

let scoreYacht (dice: Dice) : Score =
    if dice |> List.countBy id |> List.exists (fun (value, count) -> count = 5) then 50 else 0
let scoreHigh (dice: Dice) : Score =
    if dice |> List.forall (fun x -> x >= 4) then List.sum dice else 0
let scoreLow (dice: Dice) : Score =
    if dice |> List.forall (fun x -> x <= 3) then List.sum dice else 0
let scoreEvens (dice: Dice) : Score =
    if dice |> List.forall (fun x -> x % 2 = 0) then List.sum dice else 0
let scoreOdds (dice: Dice) : Score =
    if dice |> List.forall (fun x -> x % 2 <> 0) then List.sum dice else 0
let scoreZero (dice: Dice) : Score = 0

let scoreCalculation (category: ScoreCategory) (dice: Dice) : Score =
    match category with
    | Ones -> scoreOnes dice
    | Twos -> scoreTwos dice
    | Threes -> scoreThrees dice
    | Fours -> scoreFours dice
    | Fives -> scoreFives dice
    | Sixes -> scoreSixes dice
    | Choice -> scoreChoice dice
    | FourOfAKind -> scoreFourOfAKind dice
    | FullHouse -> scoreFullHouse dice
    | SmallStraight -> scoreSmallStraight dice
    | LargeStraight -> scoreLargeStraight dice
    | High -> scoreHigh dice
    | Low -> scoreLow dice
    | Evens -> scoreEvens dice
    | Odds -> scoreOdds dice
    | Zero -> scoreZero dice
    | Yacht -> scoreYacht dice
