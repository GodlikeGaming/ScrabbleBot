module Dictionary

    type Dictionary =
        | Node of bool * Map<char,Dictionary>


    let empty (s:string) =
        let m = Map<char, Dictionary>
        let q = Map<int, string>
        Node (false, Map.empty)

    let rec insert (s:string) =
        function
        | Node (_,d)        when s.Length = 0 -> Node(true, d)
        | Node (b,d)                          -> match Map.tryFind (s.Chars(0)) d with
                                                    | Some dict -> Node (b, d |> Map.add (s.Chars(0)) (insert (s.Remove(0, 1)) (dict)) )
                                                    | None      -> Node (b, d |> Map.add (s.Chars(0)) (insert (s.Remove(0, 1)) (empty "")))
        

    let rec lookup (s:string) =
        function
        | Node (b, _) when      s.Length = 0                           -> b
        | Node (_, d) when      d.ContainsKey (s.Chars(0))             -> match (Map.tryFind (s.Chars(0))) d with
                                                                            | Some dict -> lookup (s.Remove(0,1)) dict
                                                                            | None -> false
        | _                                                             -> false

    let rec contains (d : Map<char, Dictionary>) = 
        function 
        | []    -> false
        | x::xs -> printf "checking if dict contains %A\n" (x::xs); 
                   match (d.ContainsKey (x)) with 
                    | true -> true
                    | false -> contains d xs

    let rec find (map : Map<char, Dictionary>) = 
        function 
        | []        -> (None, None)
        | x::xs     -> match (Map.tryFind (x)) map with
                          | Some dict -> (Some x, Some dict)
                          | None -> find map xs


   


    
    let charListToString (lst : char list)= List.fold (fun acc x -> string x + acc) "" lst

    let getKeys = 
        function 
        | Node (_, d) -> Map.fold (fun acc key value -> key::acc) [] d

    let rec tabs =
        function
        | 0     -> ""
        | count -> "\t" + tabs (count-1)

    let checkNode dict = 
        match dict with
        | Node (b, _) -> b
(*
    let findWord d pieces = 
        let rec aux word c_node i = 
            function 
            | []                            -> printf "tried all letters and found no word." ; None
            | x::xs     when i = List.length (xs)    -> printf "%stried all pieces in %A. removing: %A\n" (tabs ((List.length pieces) - List.length (x::xs))) (List.rev word) word.Head ; None
            | x::xs     -> printf "%schecking node: %A\n" (tabs ((List.length pieces) - List.length (x::xs))) x ; 
                           match (traverse x c_node) with
                           | Some (b, n_node) when b  -> printf "%sfound word: %s\n" (tabs ((List.length pieces) - List.length (x::xs))) ((charListToString word) + string x);  Some (bestWord (x::word) (aux word d (i+1) (xs @ [x])))
                           | Some(_, c_node)          -> printf "%sentering branch: %A\n" (tabs ((List.length pieces) - List.length (x::xs))) ((charListToString word) + string x) ; 
                                                       match (aux (x::word) node (0) (xs)) with
                                                       | Some word  -> Some (bestWord (x::word) (aux word d (i+1) (xs @ [x])))
                                                       | None       -> printf "%sfound nothing in %A\n%sreturning to %A\n" (tabs ((List.length pieces) - List.length (x::xs))) ((charListToString word) + string x) (tabs ((List.length pieces) - List.length (x::xs) - 1 )) (charListToString word) ; None // aux word node (i+1) (xs @ [x])
                           | None       -> printf "%s%A not found in %A\n" (tabs ((List.length pieces) - List.length (x::xs))) x (getKeys d)  ; aux word d (i+1) (xs @ [x])
            | x::xs     when checkNode d                -> printf "%sfound: %s, trying neighbours\n" (tabs ((List.length pieces) - List.length (x::xs))) (charListToString word) ; Some (bestWord word (aux word d (i+1) (xs @ [x])))
            
        aux [] d 0 pieces

*)
    let getChildren d = Map.fold(fun acc key value -> (key, value)::acc) [] d

    let first (a, _, _) = a
    let second (_, b, _) = b
    let third (_, _, c) = c

    let calculatePoints lst ws =
        let rec aux i =
            function
            | []    ->  []
            | x::xs -> 
                        let kv = Map.fold (fun acc key value -> (key, (value i ws)) :: acc) [] (snd x)
                        kv @ (aux (i+1u) xs)

        let functions = aux 0u lst   
        let sorted = List.sortBy (fun x -> fst x) functions 
        let sum = List.fold (fun acc x -> acc |> (snd x)) 0 sorted
        sum

    let findBest2 w1 w2 (board : ScrabbleUtil.board) = 
        let w1_score = calculatePoints (List.fold (fun acc x -> Option.get (board.tiles (fst x)) :: acc) [] w1) (List.toArray(List.map (fun x -> (second (snd x), third (snd x))) w1))
        let w2_score = calculatePoints (List.fold (fun acc x -> Option.get (board.tiles (fst x)) :: acc) [] w2) (List.toArray(List.map (fun x ->(second (snd x), third (snd x))) w2))
        if w1_score > w2_score then
            w1
        else 
            w2

    let bestWord lst board = 
        let mutable best = []
        for entry in lst do 
            best <- findBest2 entry best board
        best


    let remove c lst =
        let mutable prev = []
        let rec aux =   
            function 
            | []                    -> []
            | x::xs when fst x = c  ->  xs
            | x::xs                 -> prev <- prev @ [x] ; aux xs      
        let res = aux lst 
        prev @ res

    
    let traverse piece = 
        function
        | Node (b, d) when Map.containsKey (second piece) d  -> Some ((first piece, second piece, third piece), Map.find (second piece) d)      
        | _             -> None 

(*     let traverse set  = 
        function
        | Node (b, d)   ->      let mutable found = None
                                for entry in snd set do
                                    match Map.tryFind (fst entry) d with
                                    | Some n -> found <- Some ((fst set, fst entry, snd entry), n)
                                    | None -> ()
                                found
                                
        | _             -> None  *)

    let l_first =
        function 
        | [] -> failwith "empty list bro... ffs"
        | x::xs -> x

    let word_str word = List.fold(fun acc x -> acc + string (second (snd x))) "" (List.rev word)

    let rec next_availible n xs =
        if List.isEmpty xs then 
            true
        else
            let x::xs' = xs in 
            if n = 0 then 
                match snd x with 
                | Some x -> false
                | None -> true 
            else 
                next_availible (n - 1) xs'

    //Now check the word by going left right up and down
    let legalSpot lp word dir n_pos c d  = 

        //if List.isEmpty word then true else 

        let str listOfChars = System.String (listOfChars |> List.toArray)

        let rec aux dir pos = 
            let new_pos = (fst pos + fst dir, snd pos + snd dir)
            match Map.tryFind new_pos lp with
            | Some x    -> fst x::aux dir new_pos
            | None -> [] 
        // find word going left and right

        let up = aux (0, -1) n_pos
        let down = aux (0, 1) n_pos
        let word_vertical = (List.rev up @ [c] @ down)
        let left = aux (-1, 0) n_pos 
        let right = aux (1, 0) n_pos
        let word_horizontal = (List.rev right @ [c] @ left) |> List.rev




        let res_vertical = lookup (str word_vertical) d || word_vertical.Length = 1

        let res_horizontal = lookup (str word_horizontal) d || word_horizontal.Length = 1

        let temp = word_str word


       
        if fst dir = 1 then 
            if not (List.isEmpty right) then
                res_vertical && (lookup (temp + str word_horizontal) d || word_horizontal.Length = 1)
            else
                res_vertical
        else 
            if not (List.isEmpty down) then
                res_horizontal && (lookup (temp + str word_vertical) d || word_vertical.Length = 1)
            else 
                res_horizontal 

    let getPieces set = List.fold(fun acc x -> (Set.fold(fun acc2 x2 -> (fst x, fst x2, snd x2)::acc2) [] (snd x)) @ acc) [] set

    let getNodes node (line : 'a list) lst = (List.fold (fun acc x -> (traverse x node)::acc) [] lst) |> List.filter(fun x -> x.IsSome) |> List.map(Option.get) |> List.fold (fun acc x -> (fst line.Head, x)::acc) [] 

    let findWord root p (line :'a list) dir lp board = 

        let rec aux word p used_piece used_placed_piece (line : 'a list) n =
            //printf "%A\n" (word |> List.rev |> List.map(fun x -> second(snd x))) ;
            let res = (word |> List.map(fun x -> second(snd x)) |> List.fold (fun acc x -> string x+acc) "" )

            
            if line.IsEmpty || List.isEmpty p then 
                match n with 
                | Node (b, _) when b && used_piece && used_placed_piece ->  word
                | _ -> []                                                                   //(aux ((fst x, fst (snd x))::word) p true used_placed_piece line.Tail (snd (snd x))) :: acc
            else  
                match n, (snd line.Head), used_piece, (used_placed_piece || Map.count lp = 0) with
                | Node (b, d), Some x, true, true   when b    ->  
                                                            match (Map.tryFind (second x) d), lookup (word_str word) root, legalSpot lp word dir (fst line.Head) (x |> second) root with 
                                                            | Some node, true, true -> bestWord (word::[(aux ((fst line.Head,  Option.get (snd line.Head))::word) p used_piece true line.Tail node)]) board
                                                            | Some node, _, true    -> (aux ((fst line.Head,  Option.get (snd line.Head))::word) p used_piece true line.Tail node)
                                                            | _, true, true         -> word
                                                            | _               -> []
                                                            
                | Node (_, d), Some x, _, _             ->  
                                                            match (Map.tryFind (second x) d), legalSpot lp word dir (fst line.Head) (x |> second) root with 
                                                            | Some node, true -> aux ((fst line.Head,  Option.get (snd line.Head))::word) p used_piece true line.Tail node
                                                            | _               -> []

                | Node (b, d), None, true, true  when b    ->  //bestWord (word::((getPieces p) |> (getNodes n line) |> List.filter(fun x -> legalSpot lp word dir (fst line.Head) (x |> snd |> fst |> second) root) |> (List.fold (fun acc x -> (aux ((fst x, fst (snd x))::word) (remove (x |> snd |> fst |> first) p) true used_placed_piece line.Tail (snd (snd x))) :: acc) [] )) )
                                                                let pieces = getPieces p
                                                                let nodes = getNodes n line pieces
                                                                let filtered = List.filter(fun x -> legalSpot lp word dir (fst x) (x |> snd |> fst |> second) root) nodes
                                                                let words = List.fold (fun acc x -> (aux ((fst x, fst (snd x))::word) (remove (x |> snd |> fst |> first) p) true used_placed_piece line.Tail (snd (snd x))) :: acc) [] filtered
                                                                let w = bestWord (word::words) board
                                                                w
                | Node (_, d), None, _, _                  ->  //bestWord ((getPieces p) |> (getNodes n line) |> List.filter(fun x -> legalSpot lp word dir (fst x) (x |> snd |> fst |> second) root) |> (List.fold (fun acc x -> (aux ((fst x, fst (snd x))::word) (remove (x |> snd |> fst |> first) p) true used_placed_piece line.Tail (snd (snd x))) :: acc) [] ))
                                                                let pieces = getPieces p
                                                                let nodes = getNodes n line pieces
                                                                let filtered = List.filter(fun x -> legalSpot lp word dir (fst x) (x |> snd |> fst |> second) root) nodes
                                                                let words = List.fold (fun acc x -> (aux ((fst x, fst (snd x))::word) (remove (x |> snd |> fst |> first) p) true used_placed_piece line.Tail (snd (snd x))) :: acc) [] filtered
                                                                let w = bestWord words board
                                                                w
                | _                                     ->  []
        aux [] p false false line root  
    
(* 
    let findWord_old root p line dir lp = 
        let rec aux word p used_piece used_placed_piece (line : 'a list) n =
            let mutable temp = None
            if line.IsEmpty then
                match n with 
                | Node (b, _) when b && used_piece && used_placed_piece -> word //add more rules here
                | _                  -> []
            else      // && (legalSpot lp (second (snd word.Head)) (fst line.Head) root)
                match n, (snd line.Head), used_piece, (used_placed_piece || Map.count lp = 0), (next_availible 1 line) with 
                //| _ when not (legalSpot lp (second (snd word.Head)) (fst line.Head) root) -> []
              
                
                | Node (_, d), Some x, _, _, _  -> match Map.tryFind (second x) d  with 
                                                        | Some leaf -> (aux ((fst line.Head, x)::word) p used_piece true line.Tail leaf  )
                                                        | None -> []
                | Node (b, _) as node, None, true, true, true when b ->  

                                                                        let mutable words = []
                                                                        for entry in p do  
                                                                            match (traverse entry node), (legalSpot lp word dir (fst line.Head) root line) with
                                                                                | _, false -> ()
                                                                                | Some leaf, _ -> words <- (aux ((fst line.Head, fst leaf)::word) (remove entry p) true used_placed_piece line.Tail (snd leaf))::words
                                                                                | _ -> ()
                                                                        words <- word::words
                                                                        bestWord (words)
                                                                              // bestWord (word::(List.fold (fun acc x -> (temp <- fst(traverse x node)) ; (aux ((fst line.Head, temp)::word) (remove x p) true used_placed_piece line.Tail (snd (traverse x node))) :: acc ) [] p )) // (legalSpot lp (second (snd word.Head)) (fst line.Head) root)
                //| Node (b, d) as node, None, true when b -> bestWord (word::(List.fold (fun acc x -> (temp <- fst(traverse x node)) ; (aux ((fst line.Head, temp)::word) (remove x p) true line.Tail (snd (traverse x node))) :: acc ) [] p ))
                    | Node (b, _) as node, None, _, _, _   ->       
                                                                    let mutable words = []
                                                                    for entry in p do  
                                                                        match (traverse entry node), (legalSpot lp word dir (fst line.Head) root line) with
                                                                          | _, false -> ()
                                                                          | Some leaf, _ -> words <- (aux ((fst line.Head, fst leaf)::word) (remove entry p) true used_placed_piece line.Tail (snd leaf))::words
                                                                          | _ -> ()
                                                                    bestWord words 
                                                                           //bestWord (List.fold (fun acc x -> (temp <- fst(traverse x node)) ; (aux ((fst line.Head, temp)::word) (remove x p) true used_placed_piece line.Tail (snd (traverse x node))) :: acc ) [] p )
                | _ -> []
        aux [] p false false line root   
     *)

    // root p line dir lp 
    let findWord2 root p lines lp board = bestWord (List.filter(fun x -> List.length x > 0) (List.fold(fun acc line -> (findWord root p (snd line) (fst line) lp board) :: acc) [] lines)) board


                        (*
    let rec traverseCheck (word: char list) (words: string list) (pieces: (char * int) list)=
        function
        | Node (b, _) when      pieces.Length = 0 && not b                  -> None
        | Node (b, _) when      pieces.Length = 0 && b                      -> Some (word, words)
        | Node (b, d)                                                       -> Some (word, words)

        | Node (_, d)       -> Some (word, List.fold (fun acc x -> Option.get (traverseCheck (x::word) words pieces) :: acc) words pieces)  
        | _                                                              -> None
        *)

  
    
        (*
    let rec traverse (word: char list) (lst: char list) =
        function
        | Node (b, _) when      lst.Length = 0 && not b                  -> None
        | Node (b, _) when      lst.Length = 0 && b                      -> Some (List.rev word)
        | Node (b, d) when      contains d lst && b                      -> Some (List.rev word)
        | Node (_, d) when      contains d lst                           ->  
                                                                              match (find d lst) with
                                                                                | (x, Some dict) -> traverse ((Option.get x)::word) (remove (Option.get x) lst) dict
                                                                                | _              -> printf "Didn't find any of the letters: %A , in node" lst ; None

        | _                                                             -> None

        *)