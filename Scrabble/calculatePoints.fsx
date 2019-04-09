module CalculatePoints 
    type tile = char * Map<uint32, uint32 -> (char * int)[] -> int -> int>


    //let calculatePoints lst ws = List.fold(fun acc x ->  (ws.[int 0] |> snd |> (snd lst) ) ) Map.empty lst //Map.fold (fun acc x -> ) (snd lst)

    //let findmin (map : Map<_,_>) = map |> Seq.minBy (fun kvp -> kvp.Key)

    (* let calculatePoints lst ws = 
                                let i = ref 0
                                fun p -> List.fold(fun acc x -> Map.add (fst lst) ((snd lst) (i |> incr) ws p) acc ) Map.empty lst


                                *)

 (*   let calculatePoints lst ws = 
                            let i = ref -1
                            0 |> (fun p -> List.fold(fun acc x ->  
                                                    let kv = (findmin (snd x))
                                                    Map.add kv.Key (kv.Value (uint32 i.Value) ws p) acc) Map.empty lst) 

                                                    *)
    let calculatePoints lst ws =
        let rec aux i =
            function

            | []    ->  []
            | x::xs -> 
                        let kv = Map.fold (fun acc key value -> (key, (value i ws)) :: acc) [] (snd x)//(findmin (snd x))
                        kv @ (aux (i+1u) xs)

        let functions = aux 0u lst  
        let sorted = List.sortBy (fun x -> fst x) functions |> List.rev
        //printf "%A" sorted
        let sum = List.foldBack (fun x acc -> acc |> (snd x)) sorted 0
        sum


(*
    let calculatePoints lst ws =
        let rec aux i = 
            function 
            | []    -> fun p -> 0u + p
            | x::xs -> 
                        let kv = (findmin (snd x))
                        ((kv.Value) ws i)  >> (aux (i+1u) xs)
        aux 0u lst

        *)