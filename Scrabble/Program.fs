﻿open System.IO

open ScrabbleLib

open ScrabbleServer
open ScrabbleUtil.ServerCommunication

module RegEx =
    open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let parseMove ts =
        let pattern = @"([-]?[0-9]+[ ])([-]?[0-9]+[ ])([0-9]+)([A-Z]{1})([0-9]+)[ ]?" 
        Regex.Matches(ts, pattern) |>
        Seq.cast<Match> |> 
        Seq.map 
            (fun t -> 
                match t.Value with
                | Regex pattern [x; y; id; c; p] ->
                    ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))
                | _ -> failwith "Failed (should never happen)") |>
        Seq.toList

 module Print =

    let printHand pieces hand =
        hand |>
        MultiSet.fold (fun _ x i -> printfn "%d -> (%A, %d)" x (Map.find x pieces) i) ()

    let printBoard board radius placed =

        let c = ScrabbleUtil.Board.center board

        let minX = fst c - radius
        let maxX = fst c + radius
        let minY = snd c - radius
        let maxY = snd c + radius

        for y in [minY..maxY] do
            for x in [minX..maxX] do
                match Map.tryFind (x, y) placed, ScrabbleUtil.Board.tiles board (x, y) with
                | None, Some (c, _) -> printf "%c " c
                | Some (c, _), _    -> printf "%c " c
                | _, None -> printf "# "
            printf "\n"

module State = 
    open ScrabbleUtil

    type state = {
        lettersPlaced : Map<ScrabbleUtil.coord, char * int>
        hand          : MultiSet.MultiSet<uint32>
    }

    let mkState lp h = { lettersPlaced = lp; hand = h }

    let newState hand = mkState Map.empty hand

    let lettersPlaced st = st.lettersPlaced
    let hand st          = st.hand

let readLines filePath = System.IO.File.ReadLines(filePath)

let empty_dict = Dictionary.empty "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

let dict = List.fold (fun acc x -> Dictionary.insert x acc) empty_dict (Seq.toList (readLines "/Users/Kjaerulff/Desktop/Scrabble/EnglishDictionary.txt"))


let recv play st msg =
    match msg with
    | RCM (CMPlaySuccess(ms, points, newPieces)) ->
        (* Successful play by you. Update your state *)                 

        let lp = State.lettersPlaced st
        let hand = State.hand st 

        // find letters placed after adding the letters put down by player
        let lp_new = List.fold (fun acc x -> Map.add (fst x) (snd (snd x)) acc) lp ms

        // remove placed letters from players hand
        let hand_reduced = List.fold (fun acc x -> MultiSet.removeSingle (fst (snd x)) acc) hand ms

        // add new pieces to hand
        let hand_new = List.fold (fun acc x -> MultiSet.add (fst x) (snd x) acc) hand_reduced newPieces

        // update state based on lp_new and hand_new
        let st' = State.mkState lp_new hand_new


        play st'
    | RCM (CMPlayed (pid, ms, points)) ->
        (* Successful play by other player. Update your state *)
        let st' = st // This state needs to be updated
        play st'
    | RCM (CMPlayFailed (pid, ms)) ->
        (* Failed play. Update your state *)
        let st' = st // This state needs to be updated
        play st'
    | RCM (CMGameOver _) -> ()
    | RCM a -> failwith (sprintf "not implmented: %A" a)
    | RErr err -> printfn "Server Error:\n%A" err; play st
    | RGPE err -> printfn "Gameplay Error:\n%A" err; play st


(*let rec iterateOverList (word : char list) = 
    let rec aux =  
        function 
        | []        -> None
        | x::xs     ->  match (Dictionary.traverse (fst x) dict) with
                        | Some dict -> Some (x, dict)
                        | None      -> aux xs
    function 
    | []        -> None
    | x::xs     -> match (aux (Set.toList x)) with
                   | Some (x, dict) -> match (Dictionary.checkNode dict) with
                                       | true  -> Some (fst x::word)
                                       | false -> iterateOverList (fst x::word) xs
                   | None   -> None


                   *)
let rec cList x =
   function
   | 0u      -> []
   | count  -> x::(cList x (count-1u))




let playGame send board pieces st =
    let rec aux st =
        Print.printBoard board 8 (State.lettersPlaced st)
        printfn "\n\n"
        Print.printHand pieces (State.hand st)

        printfn "Input move (format '(x-coord y-coord piece-key character point-value )*')"

        let hand = State.hand st

        let lp = State.lettersPlaced st

        let tiles_in_hand = MultiSet.fold (fun acc x i -> (cList (x, (Map.find x pieces)) i) @ acc) [] hand


        let getPos dir pos =
            let rec aux pos =
                let t_pos = (fst pos + fst dir, snd pos + snd dir)
                match pos with
                | (x, y) when Map.containsKey pos lp  -> (pos, (Map.find pos lp)) :: (aux t_pos)
                | (x, y)                              -> match board.tiles (x,y) with 
                                                         | Some tile ->  (pos, (' ', 0)) :: (aux t_pos)
                                                         | None -> []

            if (Map.containsKey (fst pos - fst dir, snd pos - snd dir) lp) then 
                []
            else 
                aux pos


        let vertical = Map.fold (fun acc pos value -> (getPos (0, 1) pos) :: acc) [] lp |> List.filter (fun x -> List.length x > 0)

        let horizontal = Map.fold (fun acc pos value -> (getPos (1, 0) pos) :: acc) [] lp |> List.filter (fun x -> List.length x > 0)

        let comb = vertical @ horizontal
        //Map.fold (fun acc key value -> Map.containsKey (key) )[] lp


        let result = Dictionary.findWord dict tiles_in_hand

        let first (a, _, _) = a
        let second (_, b, _) = b
        let third (_, _, c) = c

        printf "Found word: %A\n" (List.fold (fun acc x -> second x :: acc) [] result)

        printf "with points: %A\n" (List.fold (fun acc x -> (third x) + acc) 0 result)

        let input = 
            let mutable i = -2
            List.fold (fun acc x -> i <- i + 1 ;  "0 " + string (i) + " " + string (first x)  + string (second x) + string(third x) + " " + acc) "" (List.rev result)

        printf "%A\n" input

        //let input =  System.Console.ReadLine()
        System.Console.ReadLine() |> ignore

        let move = RegEx.parseMove input

        send (recv aux st) (SMPlay move)

    aux st



let startGame send (msg : Response) = 
    match msg with
    | RCM (CMGameStarted (board, pieces, playerNumber, hand, playerList)) ->
        let hand' = List.fold (fun acc (v, x) -> MultiSet.add v x acc) MultiSet.empty hand
        playGame send board pieces (State.newState hand')
    | _ -> failwith "No game has been started yet"



[<EntryPoint>]
let main argv =
    let send = Comm.connect ()

    send (startGame send) (SMStartGame(1u, "My game", "", "My name"))
    0 // return an integer exit code