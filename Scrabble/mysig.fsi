

module Dictionary
type Dictionary = | Node of bool * Map<char,Dictionary>
val empty : s:string -> Dictionary
val insert : s:string -> _arg1:Dictionary -> Dictionary
val lookup : s:string -> _arg1:Dictionary -> bool
val contains : d:Map<char,Dictionary> -> _arg1:char list -> bool
val find :
  map:Map<char,Dictionary> -> _arg1:char list -> char option * Dictionary option
val charListToString : lst:char list -> string
val getKeys : _arg1:Dictionary -> char list
val tabs : _arg1:int -> string
val checkNode : dict:Dictionary -> bool
val getChildren : d:Map<'a,'b> -> ('a * 'b) list when 'a : comparison
val first : a:'a * 'b * 'c -> 'a
val second : 'a * b:'b * 'c -> 'b
val third : 'a * 'b * c:'c -> 'c
val calculatePoints :
  lst:('a * Map<'b,(uint32 -> 'c -> int -> int)>) list -> ws:'c -> int
    when 'b : comparison
val findBest2 :
  int * (ScrabbleUtil.coord * ('a * char * int)) list ->
    int * (ScrabbleUtil.coord * ('a * char * int)) list ->
      board:ScrabbleUtil.board ->
        int * (ScrabbleUtil.coord * ('a * char * int)) list
val bestWord :
  lst:seq<int * (ScrabbleUtil.coord * ('a * char * int)) list> ->
    board:ScrabbleUtil.board ->
      int * (ScrabbleUtil.coord * ('a * char * int)) list
val remove : c:'a -> lst:('a * 'b) list -> ('a * 'b) list when 'a : equality
val traverse :
  'a * char * 'b -> _arg1:Dictionary -> (('a * char * 'b) * Dictionary) option
val l_first : _arg1:'a list -> 'a
val word_str : word:('a * ('b * char * 'c)) list -> string
val next_availible : n:int -> xs:('a * 'b option) list -> bool
val legalSpot :
  lp:Map<(int * int),(char * 'a)> ->
    word:('b * ('c * char * 'd)) list ->
      int * 'e -> int * int -> c:char -> d:Dictionary -> bool
val getPieces :
  set:('a * Set<'b * 'c>) list -> ('a * 'b * 'c) list
    when 'b : comparison and 'c : comparison
val getNodes :
  node:Dictionary ->
    line:('a * 'b) list ->
      lst:('c * char * 'd) list -> ('a * (('c * char * 'd) * Dictionary)) list
val findWord :
  root:Dictionary ->
    all_pieces:('a * Set<char * int>) list ->
      line:((int * int) * ('a * char * int) option) list ->
        int * 'b ->
          lp:Map<(int * int),(char * 'c)> ->
            board:ScrabbleUtil.board ->
              wordAgent:MailboxProcessor<int *
                                         (ScrabbleUtil.coord * ('a * char * int)) list> ->
                int * (ScrabbleUtil.coord * ('a * char * int)) list
    when 'a : equality
val findWord2 :
  root:Dictionary ->
    p:('a * Set<char * int>) list ->
      lines:((int * 'b) * ((int * int) * ('a * char * int) option) list) list ->
        lp:Map<(int * int),(char * 'c)> ->
          board:ScrabbleUtil.board ->
            wordAgent:MailboxProcessor<int *
                                       (ScrabbleUtil.coord * ('a * char * int)) list> ->
              (ScrabbleUtil.coord * ('a * char * int)) list when 'a : equality

module MultiSet
type MultiSet<'a when 'a : comparison> =
  | MS of Map<'a,uint32>
  with
    override ToString : unit -> string
  end
val empty : MultiSet<'a> when 'a : comparison
val isEmpty : MultiSet<'a> -> bool when 'a : comparison
val size : MultiSet<'a> -> uint32 when 'a : comparison
val contains : 'a -> MultiSet<'a> -> bool when 'a : comparison
val numItems : 'a -> MultiSet<'a> -> uint32 when 'a : comparison
val found : _arg1:uint32 option -> uint32
val add : 'a -> uint32 -> MultiSet<'a> -> MultiSet<'a> when 'a : comparison
val addSingle : 'a -> MultiSet<'a> -> MultiSet<'a> when 'a : comparison
val remove : 'a -> uint32 -> MultiSet<'a> -> MultiSet<'a> when 'a : comparison
val removeSingle : 'a -> MultiSet<'a> -> MultiSet<'a> when 'a : comparison
val fold :
  ('a -> 'b -> uint32 -> 'a) -> 'a -> MultiSet<'b> -> 'a when 'b : comparison
val foldBack :
  ('a -> uint32 -> 'b -> 'b) -> MultiSet<'a> -> 'b -> 'b when 'a : comparison
val map :
  ('a -> 'b) -> MultiSet<'a> -> MultiSet<'b>
    when 'a : comparison and 'b : comparison
val ofList : 'a list -> MultiSet<'a> when 'a : comparison
val dubList : 'a * uint32 -> 'a list
val toList : MultiSet<'a> -> 'a list when 'a : comparison
val union : MultiSet<'a> -> MultiSet<'a> -> MultiSet<'a> when 'a : comparison
val subtract : MultiSet<'a> -> MultiSet<'a> -> MultiSet<'a> when 'a : comparison
val intersection :
  MultiSet<'a> -> MultiSet<'a> -> MultiSet<'a> when 'a : comparison

module Program
module RegEx = begin
  val ( |Regex|_| ) : pattern:string -> input:string -> string list option
  val parseMove : ts:string -> ((int * int) * (uint32 * (char * int))) list
end
module Print = begin
  val printHand :
    pieces:Map<uint32,'a> -> hand:MultiSet.MultiSet<uint32> -> unit
  val printBoard :
    board:ScrabbleUtil.board ->
      radius:int -> placed:Map<(int * int),(char * 'a)> -> unit
end
module State = begin
  type state =
    {lettersPlaced: Map<ScrabbleUtil.coord,(char * int)>;
     hand: MultiSet.MultiSet<uint32>;
     player_queue: List<uint32>;
     player_id: uint32;}
  val mkState :
    lp:Map<ScrabbleUtil.coord,(char * int)> ->
      h:MultiSet.MultiSet<uint32> -> p:List<uint32> -> p_id:uint32 -> state
  val newState :
    hand:MultiSet.MultiSet<uint32> -> (List<uint32> -> uint32 -> state)
  val lettersPlaced : st:state -> Map<ScrabbleUtil.coord,(char * int)>
  val hand : st:state -> MultiSet.MultiSet<uint32>
  val push_player : queue:'a list -> p_id:'a -> 'a list
  val pop_player : st:state -> uint32 * uint32 list
end
val readLines :
  filePath:string -> System.Collections.Generic.IEnumerable<string>
val empty_dict : Dictionary.Dictionary
val dict : Dictionary.Dictionary
val cList : x:'a -> _arg1:uint32 -> 'a list
val playGame :
  cstream:System.IO.Stream ->
    board:ScrabbleUtil.board ->
      pieces:Map<uint32,Set<char * int>> -> st:State.state -> unit
val setupGame :
  cstream:System.IO.Stream ->
    board:ScrabbleUtil.board ->
      alphabet:'a -> words:'b -> handSize:'c -> timeout:'d -> unit
val joinGame :
  port:int ->
    gameId:uint32 -> password:string -> playerName:string -> Async<unit>
val startGame : port:int -> numberOfPlayers:uint32 -> Async<unit>
val main : argv:string [] -> int

