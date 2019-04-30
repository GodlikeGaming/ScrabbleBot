open System.IO

open ScrabbleServer
open ScrabbleUtil
open ScrabbleUtil.ServerCommunication

open System.Net.Sockets

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


let rec cList x =
   function
   | 0u      -> []
   | count  -> x::(cList x (count-1u))

let playGame cstream board pieces (st : State.state) =

    let rec aux (st : State.state) =
        Print.printBoard board 8 (State.lettersPlaced st)
        printfn "\n\n"
        Print.printHand pieces (State.hand st)

        printfn "Input move (format '(<x-coordinate><y-coordinate> <piece id><character><point-value> )*', note the absence of state between the last inputs)"
        
        let hand = State.hand st

        let lp = State.lettersPlaced st

        let tiles_in_hand = MultiSet.fold (fun acc x i -> (cList (x, (Map.find x pieces)) i) @ acc) [] hand

        let findKey findValue set =
            for map in set do
                match Map.tryFindKey (fun key value -> value = findValue) map with 
                | Some x -> x
                | None -> ()

 
        let getPos dir pos =
            let rec aux pos =
                let t_pos = (fst pos + fst dir, snd pos + snd dir)
                match pos with
                | (x, y) when Map.containsKey pos lp  -> (pos, Some ( 0u, fst (Map.find pos lp), snd (Map.find pos lp))) :: (aux t_pos)
                | (x, y)                              -> match board.tiles (x,y) with 
                                                         | Some tile ->  (pos, None) :: (aux t_pos)
                                                         | None -> []

            if (Map.containsKey (fst pos - fst dir, snd pos - snd dir) lp) then 
                []
            else 
                aux pos 

        
        let vertical = Map.fold (fun acc pos value -> ((0,1), (getPos (0, 1) pos)) :: acc) [] lp |> List.filter (fun x -> List.length (snd x) > 0)

        let horizontal = Map.fold (fun acc pos value -> ((1, 0), (getPos (1, 0) pos)) :: acc) [] lp |> List.filter (fun x -> List.length (snd x) > 0)

        let rec legalPositions dir pos = 
            if pos = (1, -1) then 
                ()
            let next_pos = (fst pos + fst dir, snd pos + snd dir)
            match board.tiles (pos) with
            | Some tile -> (pos, None) :: (legalPositions dir next_pos)
            | None -> []

        let vertical_extra = //Map.fold (fun acc pos value -> List.fold (fun acc x -> ((0, 1), (getPos (0, 1) (fst x))) :: acc) [] (legalPositions (0, -1) pos)) [] lp |> List.filter (fun x -> List.length (snd x) > 0)
                                let piece_positions = Map.fold (fun acc pos value -> pos::acc) [] lp 
                                let filtered_piece_positions = List.filter (fun pos -> not (Map.containsKey (fst pos, snd pos - 1) lp)) piece_positions
                                let extra_positions = List.fold (fun acc pos -> (legalPositions (0, -1) pos) @ acc) [] filtered_piece_positions
                                let positions = List.fold (fun acc x -> ((0, 1), (getPos (0, 1) (fst x))) :: acc ) [] extra_positions
                                positions |> List.filter (fun x -> List.length (snd x) > 0)
                                
        let horizontal_extra = //Map.fold (fun acc pos value -> List.fold (fun acc x -> ((1, 0), (getPos (1, 0) (fst x))) :: acc) [] (legalPositions (-1, 0) pos)) [] lp // |> List.filter (fun x -> List.length (snd x) > 0)
                                let piece_positions = Map.fold (fun acc pos value -> pos::acc) [] lp
                                let filtered_piece_positions = List.filter (fun pos -> not (Map.containsKey (fst pos - 1, snd pos) lp)) piece_positions
                                let extra_positions = List.fold (fun acc pos -> (legalPositions (-1, 0) pos) @ acc) [] filtered_piece_positions
                                let positions = List.fold (fun acc x -> ((1, 0), (getPos (1, 0) (fst x))) :: acc ) [] extra_positions
                                positions |> List.filter (fun x -> List.length (snd x) > 0)
        //let horizontal_extra = Map.fold (fun acc pos value -> (getPos (1, 0) pos) :: acc) [] lp |> List.filter (fun x -> List.length x > 0)


        let comb = vertical @ horizontal 

        // STATUS RAPPORT 1: Nu duer botten men kun fra bogstav til højre eller bogstav og så ned. Derfor skal der tilføjes flere
        // linjer til vertical og horizontal. Som starter venstre eller over ordet

     

        let mutable result = []
        if comb.Length = 0 then 
            let mutable temp = [] 
            let mutable line = []

            for i = 0 to 8 do
                line <- ((0, i), None)::line
            temp <- ((0, 1), (List.rev line))::temp
            line <- []
            for i = 0 to 8 do
                line <- ((i, 0),None)::line
            temp <- ((1, 0), (List.rev line))::temp

            result <- Dictionary.findWord2 dict tiles_in_hand temp lp board
        else 
            try
                result <- Dictionary.findWord2 dict tiles_in_hand (comb @ vertical_extra @ horizontal_extra) lp board
            with 
                | ex -> printfn "%s" (ex.ToString()) ; 

        //Map.fold (fun acc     key value -> Map.containsKey (key) )[] lp


        

        let first (a, _, _) = a
        let second (_, b, _) = b
        let third (_, _, c) = c

        printf "Found word: %A\n" (List.fold (fun acc x -> second (snd x) :: acc) [] result)

        printf "with points: %A\n" (List.fold (fun acc x -> (third (snd x)) + acc) 0 result)

        let filtered = List.filter (fun x -> not (Map.containsKey (fst (fst x), snd(fst x)) lp)) (List.rev result)

        let input = 
            List.fold (fun acc x -> string (fst (fst x)) + " " + string (snd (fst x)) + " " + string (first(snd x))  + string (second (snd x)) + string(third (snd x)) + " " + acc) "" filtered

        printf "%A\n" input

        //let input =  System.Console.ReadLine()
        //System.Console.ReadLine() |> ignore

        //let input =  System.Console.ReadLine()
        let move = RegEx.parseMove input

        printfn "Trying to play: %A" move
        send cstream (SMPlay move)
        let msg = recv cstream
        match msg with
        | RCM (CMPlaySuccess(ms, points, newPieces)) ->
            (* Successful play by you. Update your state *)
            printfn "Success!!!\n%A\n%A\n%A" ms points newPieces
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
            aux st'
        | RCM (CMPlayed (pid, ms, points)) ->
            (* Successful play by other player. Update your state *)
            let st' = st // This state needs to be updated
            aux st'
        | RCM (CMPlayFailed (pid, ms)) ->
            (* Failed play. Update your state *)
            let st' = st // This state needs to be updated
          // System.Console.ReadLine() |> ignore
            aux st'
        | RCM (CMGameOver _) -> ()
        | RCM a -> failwith (sprintf "not implmented: %A" a)
        | RErr err -> printfn "Server Error:\n%A" err; aux st
        | RGPE err -> printfn "Gameplay Error:\n%A" err; aux st


    aux st



let setupGame cstream board alphabet words handSize timeout =
    let rec aux () =
        match ServerCommunication.recv cstream with
        | RCM (CMPlayerJoined name) ->
            printfn "Player %s joined" name
            aux ()
        | RCM (CMGameStarted (playerNumber, hand, firstPlayer, pieces, players)) as msg ->
            printfn "Game started %A" msg
            let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand
            playGame cstream board pieces (State.newState handSet)
        | msg -> failwith (sprintf "Game initialisation failed. Unexpected message %A" msg)
        
    aux ()

let joinGame port gameId password playerName =
    async {
        let client = new TcpClient(sprintf "%A" (localIP ()), port)
        use cstream = client.GetStream()
        send cstream (SMJoinGame (gameId, password, playerName))

        match ServerCommunication.recv cstream with
            | RCM (CMJoinSuccess(board, numberOfPlayers, alphabet, words, handSize, timeout)) -> 
                setupGame cstream board alphabet words handSize timeout 
            | msg -> failwith (sprintf "Error joining game%A" msg)

    }

let startGame port numberOfPlayers = 
    async {
        let client = new TcpClient(sprintf "%A" (localIP ()), port)
        let cstream = client.GetStream()
        let path = "../../../EnglishDictionary.txt"
        let words = File.ReadLines path |> Seq.toList
        let board = StandardBoard.mkStandardBoard ()
        let pieces = English.pieces()
        let alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        let handSize = 7u
        let timeout = None
        let seed = None //Some 666//Some 123 //Some 12345

        System.Console.ReadLine() |> ignore
        send cstream (SMStartGame (numberOfPlayers, "My game", "password", "My name", seed, board, pieces,
                                    handSize, alphabet, words, timeout))

        let gameId =
            match ServerCommunication.recv cstream with
            | RCM (CMGameInit gameId) -> gameId
            | msg -> failwith (sprintf "Error initialising game, server sent other message than CMGameInit (should not happen)\n%A" msg)
            
        do! (async { setupGame cstream board alphabet words handSize timeout } ::
             [for i in 2u..numberOfPlayers do yield joinGame port gameId "password" ("Player" + (string i))] |>
             Async.Parallel |> Async.Ignore)
    }

[<EntryPoint>]
let main argv =
    [Comm.startServer 13000; startGame 13000 1u] |>
    Async.Parallel |>
    Async.RunSynchronously |> ignore
    0 // return an integer exit code
