(* module Dictionary

    type Dictionary

    val empty : string -> Dictionary

    val insert : string -> Dictionary -> Dictionary

    val lookup : string -> Dictionary -> bool

    val contains : Map<char, Dictionary> -> char list -> bool

    val find : Map<char, Dictionary> -> char list -> char Option * Dictionary Option

    val checkNode : Dictionary -> bool

    //val findWord : Dictionary -> (uint32 * Set<char * int>) list -> ('a * (uint32 * char * int)) list -> (uint32 * char * int) list

    val findWord2<'a, 'b, 'c, 'd when 'a : equality and 'b : equality and 'c :equality and 'd : equality> : Dictionary -> ('a * 'b) list -> ((int * 'c) * ((int * int) * ('a * char * int) option) list) list -> Map<(int * int), (char * 'd)> ->  ((int * int) * ('a * char * int)) list *)