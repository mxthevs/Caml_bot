val skip : int -> string -> string
val take_until : char -> string -> string
val take_after : char -> string -> string
val split : str:string -> c:char -> string list
val split1_exn : str:string -> c:char -> string * string
val split_on_first_space : string -> string list
val has_char : char -> string -> bool
