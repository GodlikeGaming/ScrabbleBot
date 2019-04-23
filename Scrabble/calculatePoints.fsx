module CalculatePoints 
    type tile = char * Map<uint32, uint32 -> (char * int)[] -> int -> int>


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

