module Dictionary

    type Dictionary

    val empty : string -> Dictionary

    val insert : string -> Dictionary -> Dictionary

    val lookup : string -> Dictionary -> bool

    val contains : Map<char, Dictionary> -> char list -> bool

    val find : Map<char, Dictionary> -> char list -> char Option * Dictionary Option

    val checkNode : Dictionary -> bool

    val findWord : Dictionary -> (uint32 * Set<char * int>) list -> (uint32 * char * int) list