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

    let findBest w1 w2 = 
        if (List.length w1 > List.length w2) then
            w1
        else 
            w2 

    let findBest2 w1 w2 = 
        let w1_score = List.fold (fun acc x -> third x + acc) 0 w1 
        let w2_score = List.fold (fun acc x -> third x + acc) 0 w2
        if w1_score > w2_score then
            w1
        else 
            w2

    let bestWord lst = 
        let mutable best = List.empty
        for entry in lst do 
            best <- findBest2 entry best
        best


    let rec remove c = 
        function 
        | []    -> []
        | x::xs when snd x = snd c  -> xs @ []
        | x::xs             -> remove c (xs @ [x])

    
    let traverse set = 
        let mutable found =(0u, ' ', 0)
        function
        | Node (b, d)   when  Set.exists (fun c -> (found <- (fst set, fst c, snd c)) ; d.ContainsKey(second found)) (snd set) -> (found, Map.find (second found) d)
        | _                                                              -> ((0u, ' ', 0), empty "")


    let findWord root p = 
        let rec aux (word: (uint32 * char * int) list) p =
            let mutable temp = (0u, ' ', 0)
            function 
            | Node (b, d) as node when b -> bestWord (word::(List.fold (fun acc x -> (temp <- fst(traverse x node)) ; (aux (temp::word) (remove x p) (snd (traverse x node))) :: acc ) [] p ))
            | Node (_, d) as node        -> bestWord (List.fold (fun acc x -> (temp <- fst(traverse x node)) ; (aux (temp::word) (remove x p) (snd (traverse x node))) :: acc ) [] p )
            | _ -> []
        aux [] p root   
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