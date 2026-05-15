module YachtDice.Helper

open System

let rec removeFirst (x: int) (list: int list) =
    match list with
        | [] -> []
        | hd :: tl when x = hd -> tl
        | hd :: tl -> hd :: (removeFirst x tl) 

let ListSubtract (bigList: int list) (smallList: int list): int list =
    smallList |> List.fold (fun acc x -> removeFirst x acc) bigList

let rec inputToIntList() : int list =
    let input = System.Console.ReadLine()
    let seperatedList = input.Split([| ' ' |], System.StringSplitOptions.RemoveEmptyEntries) |> Array.toList // seperate the input by space and convert to list
    let parsedOptions = 
        seperatedList |> List.map (fun x -> 
            match System.Int32.TryParse x with
            | true, v -> Some v
            | _ -> None)
    if parsedOptions |> List.exists Option.isNone then
        printfn "Invalid input. Please enter valid integers separated by spaces."
        inputToIntList() //recursively call until valid input is given
    else 
        let numberList = parsedOptions |> List.choose id // extract the valid integers from the options
        if numberList |> List.exists (fun x -> x < 1 || x > 6) then
            printfn "Invalid input. Please enter integers between 1 and 6 separated by spaces."
            inputToIntList() //recursively call until valid input is given
        else if List.length numberList > 5 then
            printfn "Invalid input. Please enter at most 5 integers separated by spaces."
            inputToIntList() //recursively call until valid input is given
        else numberList

let clearLastLine () = // Clear the last line in the console for better UI experience when rerolling dice
    Console.SetCursorPosition(0, Console.CursorTop - 1)
    printfn "%s" (String.replicate Console.WindowWidth " ")
    Console.SetCursorPosition(0, Console.CursorTop - 1)



