open YachtDice.Types
open YachtDice.Scoring
open YachtDice.GameLogic
open YachtDice.UI
open YachtDice.Helper
open YachtDice.Action

let rec gameloop (gameState: GameState) = //every turn calls gameloop with updated gameState until game over
    if isGameOver gameState then
        let finalScore = calculateFinalScore gameState.Scores
        printfn "Game Over! Your final score is: %d" finalScore
        exitMessage()
    else 
        runGameMenu(gameState) |> selectBoard |> gameloop //recursive call with updated gameState after scoring
        


[<EntryPoint>]
let main argv =
    initUI()
    let rec runmenu() =
        initMenu()
        match System.Console.ReadLine() with
        | "1" -> gameloop(initGameState())
        | "2" -> exitMessage()
        | _ -> 
            printfn "Invalid input. Please try again."
            runmenu()
    runmenu()
    0 //return 0