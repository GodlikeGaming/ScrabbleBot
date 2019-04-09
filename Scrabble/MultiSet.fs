module MultiSet

    type MultiSet<'a when 'a : comparison> = 
        |MS of Map<'a,uint32> 
        override m.ToString() = 
            match m with 
            | MS(map) -> 
                        let s = ((Map.fold (fun acc key value -> acc + "(" + key.ToString() + ", #" + (string value) + "), ") "{" map))
                        //remove ',' after the last element and add '}'
                        let x = s.Substring(0,s.Length-2) + "}"
                        sprintf "%s" x

    let empty = MS(Map.empty)

    let isEmpty (MS(ms)) = Map.isEmpty ms

    let size (MS(ms)) = Map.foldBack(fun k v s-> s + v) ms 0u

    let contains a (MS(ms)) = Map.containsKey a ms
    
    let numItems a (MS(ms)) = 
        let result = Map.tryFind a ms 
        match result with
            | Some x -> x
            | None -> 0u

    let found = 
        function
        | Some x -> x
        | None -> 0u 

    let add a n (MS(ms)) = MS((Map.add a ((numItems a (MS(ms))) + n) ms))

    let addSingle a (MS(ms)) = add a 1u (MS(ms))
    
    let remove a n (MS(ms)) = 
        if n < numItems a (MS(ms)) then
            MS(Map.add a ((numItems a (MS(ms))) - n) ms)
            else
            MS(Map.remove a ms)

    let removeSingle a (MS(ms)) = remove a 1u (MS(ms))


    let fold f acc (MS(ms)) = 
        Map.fold f acc ms


    let foldBack f  (MS(ms)) acc = 
        Map.foldBack f ms acc


    let map f (MS(ms)) =
        Map.fold (fun acc key value -> add (f key) value acc) empty ms


    let ofList (a:list<'a>) = 
        List.fold (fun acc x -> add x 1u acc) empty a

    let rec dubList =
        function
        |(x,0u) -> []
        |(x,n) -> x::dubList(x,(n-1u))

    let toList (MS(ms)) =
        fold (fun acc key value -> acc @ dubList (key, value)) [] (MS(ms))

    let union ms1 ms2 = 
        ofList(toList ms1 @ toList ms2)

    let subtract ms1 ms2 = 
        fold (fun acc key value -> remove key value acc) ms1 ms2

    let intersection ms1 ms2 = 
        subtract ms1 (subtract ms1 ms2)
    
    
