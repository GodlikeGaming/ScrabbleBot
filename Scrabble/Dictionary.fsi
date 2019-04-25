module Dictionary

    type Dictionary

    val empty : string -> Dictionary

    val insert : string -> Dictionary -> Dictionary

    val lookup : string -> Dictionary -> bool

    val contains : Map<char, Dictionary> -> char list -> bool

    val find : Map<char, Dictionary> -> char list -> char Option * Dictionary Option

    val checkNode : Dictionary -> bool

    //val findWord : Dictionary -> (uint32 * Set<char * int>) list -> ('a * (uint32 * char * int)) list -> (uint32 * char * int) list

    val findWord2 : Dictionary -> (uint32 * Set<char * int>) list -> ((int * int) * (uint32 * char * int) option) list list -> Map<(int * int), (char * 'a)> ->  ((int * int) * (uint32 * char * int)) list