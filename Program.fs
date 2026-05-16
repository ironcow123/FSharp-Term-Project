open YachtDice.Types
open YachtDice.Scoring
open YachtDice.GameLogic
open YachtDice.UI
open YachtDice.Helper
open YachtDice.Action

let rec gameloop (gameState: GameState) = //every turn calls gameloop with updated gameState until game over
    if isGameOver gameState then
        let finalScore = calculateFinalScore gameState.Scores
        printfn ""
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
        | "1" ->
            clearLastLine() 
            gameloop(initGameState())
        | "2" -> 
            clearLastLine()
            exitMessage()
        | _ -> 
            clearLastLine()
            printfn "Invalid input. Please try again."
            printfn ""
            runmenu()
    runmenu()
    0 //return 0