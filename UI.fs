module YachtDice.UI

open System.Threading
open YachtDice.Types
open YachtDice.GameLogic

let initUI() = 
    printfn """

    __   __       _   _     ___  _          _  __   _   ___ ___ _____   ___    _ _ _   _          
    \ \ / /_ _ __| |_| |_  |   \(_)__ ___  | |/ /  /_\ |_ _/ __|_   _| | __|__| (_) |_(_)___ _ _  
     \ V / _` / _| ' \  _| | |) | / _/ -_) | ' <  / _ \ | |\__ \ | |   | _|/ _` | |  _| / _ \ ' \ 
      |_|\__,_\__|_||_\__| |___/|_\__\___| |_|\_\/_/ \_\___|___/ |_|   |___\__,_|_|\__|_\___/_||_|
                                                                                               
    """
    printfn "Welcome to Yacht Dice KAIST Edition!"
    printfn "You will have 17 turns to score in each category."
    printfn "In each turn, you can roll the dice up to 3 times and choose which dice to keep."
    printfn "After rolling, you must choose a empty category to score your dice."
    printfn "Let's get started!"
    printfn ""

let initMenu() =
    printfn "Please select an option:"
    printfn "1. Start New Game"
    printfn "2. Exit"

let exitMessage() =
    printfn "Thank you for playing Yacht Dice KAIST Edition! I hope you enjoyed it!"
    printfn ""
    Thread.Sleep(1500)
    exit 0

let gameMenu(gameState: GameState) =
    printfn ""
    printfn "Current Turn: %d" (gameState.TurnCount)
    printfn "Please select an option:"
    printfn "1. Roll Dice"
    printfn "2. Scoreboard"
    printfn "3. Exit Game"
    printfn ""

let scoreMenu() =
    printfn ""
    printfn "Please select a category to score:"
    printfn "1. Ones"
    printfn "2. Twos"
    printfn "3. Threes"
    printfn "4. Fours"
    printfn "5. Fives"
    printfn "6. Sixes"
    printfn "7. Choice"
    printfn "8. Four of a Kind"
    printfn "9. Full House"
    printfn "10. Small Straight"
    printfn "11. Large Straight"
    printfn "12. High"
    printfn "13. Low"
    printfn "14. Evens"
    printfn "15. Odds"
    printfn "16. Zero"
    printfn "17. Yacht"
    printfn ""

let showScores (scores: Map<ScoreCategory, Score option>) =
    printfn "Current Scores:"
    scores |> Map.iter (fun category scoreOption ->
        let scoreStr = match scoreOption with
                        | Some score -> string score
                        | None -> "Not scored yet"
        printfn "%A: %s" category scoreStr
    )
    printfn "Bonus Score: %d" (calculateBonusScore(scores))

let decideReroll(gameState: GameState) =
    printfn "Do you want to reroll? (reroll count: %d)" (gameState.RollCount)
    printfn "1. Yes"
    printfn "2. No"

let showDice (dice: int list) =
    printfn "Current Dice: %A" dice

 