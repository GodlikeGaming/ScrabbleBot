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
        | _                                                            -> false