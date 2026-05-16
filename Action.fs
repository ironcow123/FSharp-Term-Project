module YachtDice.Action

open YachtDice.Types
open YachtDice.Scoring
open YachtDice.GameLogic
open YachtDice.UI
open YachtDice.Helper

let rec runRollDice(gamestate: GameState): GameState =
    printfn "You have %d rolls left" gamestate.RollCount
    printfn ""
    match System.Console.ReadLine() with
    | "1" when gamestate.RollCount > 0 -> 
        clearLastLine()
        printfn "You decided to reroll. Please enter the indices of the dice you want to reroll, separated by spaces:"
        let rerollIndices = inputToIntList()
        if List.isEmpty rerollIndices then
            printfn "No dice selected for reroll. Please try again."
            runRollDice gamestate
        else if isValidReroll gamestate.Dice rerollIndices then 
            let newDice = rerollDice gamestate.Dice rerollIndices
            showDice newDice
            let newGameState = {gamestate with Dice = newDice; RollCount = gamestate.RollCount - 1}
            decideReroll newGameState
            runRollDice newGameState
        else 
            printfn "Invalid input. Please try again."
            decideReroll gamestate
            runRollDice gamestate
    | "1" -> 
        clearLastLine()
        printfn "You have no rolls left. Please choose a category to score."; //hold
        gamestate
    | "2" -> 
        clearLastLine()
        printfn "You decided to hold. Please choose a category to score."
        gamestate
    | _ -> 
        printfn "Invalid input. Please try again."
        runRollDice gamestate

let rec runGameMenu(gamestate: GameState): GameState =
        gameMenu(gamestate)
        match System.Console.ReadLine() with
            | "1" -> 
                clearLastLine()
                let newDice = rollDice()
                showDice newDice
                let newGameState = {gamestate with Dice = newDice}
                decideReroll(newGameState)
                runRollDice newGameState
            | "2" -> 
                clearLastLine()
                showScores gamestate.Scores
                runGameMenu gamestate
            | "3" -> exitMessage()
            | _ -> 
                clearLastLine()
                printfn "Invalid input. Please try again."
                runGameMenu gamestate

let rec selectBoard(gamestate: GameState): GameState =
    //scoreMenu()
    showScores gamestate.Scores
    match System.Console.ReadLine() with
    | "1" -> 
        clearLastLine()
        if isValidCategory gamestate Ones then
            let newscore = gamestate.Scores |> Map.add Ones (Some (scoreOnes gamestate.Dice)) //new map
            let newGameState = {gamestate with Scores = newscore; TurnCount = gamestate.TurnCount + 1; RollCount = 2}
            printfn "You scored %d points in Ones category." (Option.defaultValue 0 (newscore |> Map.tryFind Ones |> Option.flatten))
            newGameState //return new game state with updated score and turn count
        else 
            printfn "Category already scored. Please select another category."
            selectBoard gamestate
    | "2" -> 
        clearLastLine()
        if isValidCategory gamestate Twos then
            let newscore = gamestate.Scores |> Map.add Twos (Some (scoreTwos gamestate.Dice))
            let newGameState = {gamestate with Scores = newscore; TurnCount = gamestate.TurnCount + 1; RollCount = 2}
            printfn "You scored %d points in Twos category." (Option.defaultValue 0 (newscore |> Map.tryFind Twos |> Option.flatten))
            newGameState
        else 
            printfn "Category already scored. Please select another category."
            selectBoard gamestate
    | "3" -> 
        clearLastLine()
        if isValidCategory gamestate Threes then
            let newscore = gamestate.Scores |> Map.add Threes (Some (scoreThrees gamestate.Dice))
            let newGameState = {gamestate with Scores = newscore; TurnCount = gamestate.TurnCount + 1; RollCount = 2}
            printfn "You scored %d points in Threes category." (Option.defaultValue 0 (newscore |> Map.tryFind Threes |> Option.flatten))
            newGameState
        else 
            printfn "Category already scored. Please select another category."
            selectBoard gamestate
    | "4" -> 
        clearLastLine()
        if isValidCategory gamestate Fours then
            let newscore = gamestate.Scores |> Map.add Fours (Some (scoreFours gamestate.Dice))
            let newGameState = {gamestate with Scores = newscore; TurnCount = gamestate.TurnCount + 1; RollCount = 2}
            printfn "You scored %d points in Fours category." (Option.defaultValue 0 (newscore |> Map.tryFind Fours |> Option.flatten))
            newGameState
        else 
            printfn "Category already scored. Please select another category."
            selectBoard gamestate
    | "5" -> 
        clearLastLine()
        if isValidCategory gamestate Fives then
            let newscore = gamestate.Scores |> Map.add Fives (Some (scoreFives gamestate.Dice))
            let newGameState = {gamestate with Scores = newscore; TurnCount = gamestate.TurnCount + 1; RollCount = 2}
            printfn "You scored %d points in Fives category." (Option.defaultValue 0 (newscore |> Map.tryFind Fives |> Option.flatten))
            newGameState
        else 
            printfn "Category already scored. Please select another category."
            selectBoard gamestate
    | "6" -> 
        clearLastLine()
        if isValidCategory gamestate Sixes then
            let newscore = gamestate.Scores |> Map.add Sixes (Some (scoreSixes gamestate.Dice))
            let newGameState = {gamestate with Scores = newscore; TurnCount = gamestate.TurnCount + 1; RollCount = 2}
            printfn "You scored %d points in Sixes category." (Option.defaultValue 0 (newscore |> Map.tryFind Sixes |> Option.flatten))
            newGameState
        else 
            printfn "Category already scored. Please select another category."
            selectBoard gamestate
    | "7" -> 
        clearLastLine()
        if isValidCategory gamestate Choice then
            let newscore = gamestate.Scores |> Map.add Choice (Some (scoreChoice gamestate.Dice))
            let newGameState = {gamestate with Scores = newscore; TurnCount = gamestate.TurnCount + 1; RollCount = 2}
            printfn "You scored %d points in Choice category." (Option.defaultValue 0 (newscore |> Map.tryFind Choice |> Option.flatten))
            newGameState
        else 
            printfn "Category already scored. Please select another category."
            selectBoard gamestate
    | "8" -> 
        clearLastLine()
        if isValidCategory gamestate FourOfAKind then
            let newscore = gamestate.Scores |> Map.add FourOfAKind (Some (scoreFourOfAKind gamestate.Dice))
            let newGameState = {gamestate with Scores = newscore; TurnCount = gamestate.TurnCount + 1; RollCount = 2}
            printfn "You scored %d points in Four of a Kind category." (Option.defaultValue 0 (newscore |> Map.tryFind FourOfAKind |> Option.flatten))
            newGameState
        else 
            printfn "Category already scored. Please select another category."
            selectBoard gamestate
    | "9" -> 
        clearLastLine()
        if isValidCategory gamestate FullHouse then
            let newscore = gamestate.Scores |> Map.add FullHouse (Some (scoreFullHouse gamestate.Dice))
            let newGameState = {gamestate with Scores = newscore; TurnCount = gamestate.TurnCount + 1; RollCount = 2}
            printfn "You scored %d points in Full House category." (Option.defaultValue 0 (newscore |> Map.tryFind FullHouse |> Option.flatten))
            newGameState
        else 
            printfn "Category already scored. Please select another category."
            selectBoard gamestate
    | "10" -> 
        clearLastLine()
        if isValidCategory gamestate SmallStraight then
            let newscore = gamestate.Scores |> Map.add SmallStraight (Some (scoreSmallStraight gamestate.Dice))
            let newGameState = {gamestate with Scores = newscore; TurnCount = gamestate.TurnCount + 1; RollCount = 2}
            printfn "You scored %d points in Small Straight category." (Option.defaultValue 0 (newscore |> Map.tryFind SmallStraight |> Option.flatten))
            newGameState
        else 
            printfn "Category already scored. Please select another category."
            selectBoard gamestate
    | "11" -> 
        clearLastLine()
        if isValidCategory gamestate LargeStraight then
            let newscore = gamestate.Scores |> Map.add LargeStraight (Some (scoreLargeStraight gamestate.Dice))
            let newGameState = {gamestate with Scores = newscore; TurnCount = gamestate.TurnCount + 1; RollCount = 2}
            printfn "You scored %d points in Large Straight category." (Option.defaultValue 0 (newscore |> Map.tryFind LargeStraight |> Option.flatten))
            newGameState
        else 
            printfn "Category already scored. Please select another category."
            selectBoard gamestate
    | "12" -> 
        clearLastLine()
        if isValidCategory gamestate High then
            let newscore = gamestate.Scores |> Map.add High (Some (scoreHigh gamestate.Dice))
            let newGameState = {gamestate with Scores = newscore; TurnCount = gamestate.TurnCount + 1; RollCount = 2}
            printfn "You scored %d points in High category." (Option.defaultValue 0 (newscore |> Map.tryFind High |> Option.flatten))
            newGameState
        else 
            printfn "Category already scored. Please select another category."
            selectBoard gamestate
    | "13" -> 
        clearLastLine()
        if isValidCategory gamestate Low then
            let newscore = gamestate.Scores |> Map.add Low (Some (scoreLow gamestate.Dice))
            let newGameState = {gamestate with Scores = newscore; TurnCount = gamestate.TurnCount + 1; RollCount = 2}
            printfn "You scored %d points in Low category." (Option.defaultValue 0 (newscore |> Map.tryFind Low |> Option.flatten))
            newGameState
        else 
            printfn "Category already scored. Please select another category."
            selectBoard gamestate
    | "14" -> 
        clearLastLine()
        if isValidCategory gamestate Evens then
            let newscore = gamestate.Scores |> Map.add Evens (Some (scoreEvens gamestate.Dice))
            let newGameState = {gamestate with Scores = newscore; TurnCount = gamestate.TurnCount + 1; RollCount = 2}
            printfn "You scored %d points in Evens category." (Option.defaultValue 0 (newscore |> Map.tryFind Evens |> Option.flatten))
            newGameState
        else 
            printfn "Category already scored. Please select another category."
            selectBoard gamestate
    | "15" -> 
        clearLastLine()
        if isValidCategory gamestate Odds then
            let newscore = gamestate.Scores |> Map.add Odds (Some (scoreOdds gamestate.Dice))
            let newGameState = {gamestate with Scores = newscore; TurnCount = gamestate.TurnCount + 1; RollCount = 2}
            printfn "You scored %d points in Odds category." (Option.defaultValue 0 (newscore |> Map.tryFind Odds |> Option.flatten))
            newGameState
        else 
            printfn "Category already scored. Please select another category."
            selectBoard gamestate
    | "16" -> 
        clearLastLine()
        if isValidCategory gamestate Zero then
            let newscore = gamestate.Scores |> Map.add Zero (Some (scoreZero gamestate.Dice))
            let newGameState = {gamestate with Scores = newscore; TurnCount = gamestate.TurnCount + 1; RollCount = 2}
            printfn "You scored %d points in Zero category." (Option.defaultValue 0 (newscore |> Map.tryFind Zero |> Option.flatten))
            newGameState
        else 
            printfn "Category already scored. Please select another category."
            selectBoard gamestate
    | "17" -> 
        clearLastLine()
        if isValidCategory gamestate Yacht then
            let newscore = gamestate.Scores |> Map.add Yacht (Some (scoreYacht gamestate.Dice))
            let newGameState = {gamestate with Scores = newscore; TurnCount = gamestate.TurnCount + 1; RollCount = 2}
            printfn "You scored %d points in Yacht category." (Option.defaultValue 0 (newscore |> Map.tryFind Yacht |> Option.flatten))
            newGameState
        else 
            printfn "Category already scored. Please select another category."
            selectBoard gamestate
    | _ ->
        printfn "Invalid input. Please select number between 1 and 17."
        selectBoard gamestate