(* A stand-in for the pprint library, which is not installed here.  Sek uses
   it only for its debugging printers, so a document is just a string and the
   combinators concatenate. *)

type document = string

let empty = ""
let string s = s
let utf8string s = s
let utf8format fmt = Printf.ksprintf (fun s -> s) fmt
let char c = String.make 1 c
let hardline = "\n"
let break _ = " "
let space = " "
let comma = ","
let semi = ";"
let dot = "."
let lparen = "("
let rparen = ")"
let lbracket = "["
let rbracket = "]"
let lbrace = "{"
let rbrace = "}"
let langle = "<"
let rangle = ">"
let bar = "|"
let equals = "="
let colon = ":"
let at = "@"
let underscore = "_"
let dquote = "\""
let squote = "'"
let ampersand = "&"
let star = "*"
let plus = "+"
let minus = "-"
let slash = "/"
let backslash = "\\"
let bang = "!"
let qmark = "?"
let percent = "%"
let caret = "^"
let tilde = "~"
let sharp = "#"
let dollar = "$"

let ( !^ ) s = s
let ( ^^ ) a b = a ^ b
let ( ^/^ ) a b = a ^ " " ^ b
let ( ^//^ ) a b = a ^ " " ^ b

let group d = d
let nest _ d = d
let align d = d
let hang _ d = d
let prefix _ _ a b = a ^ " " ^ b
let infix _ _ op a b = a ^ " " ^ op ^ " " ^ b
let surround _ _ o d c = o ^ d ^ c
let precede o d = o ^ d
let terminate c d = d ^ c
let enclose o c d = o ^ d ^ c
let parens d = "(" ^ d ^ ")"
let brackets d = "[" ^ d ^ "]"
let braces d = "{" ^ d ^ "}"
let angles d = "<" ^ d ^ ">"
let dquotes d = "\"" ^ d ^ "\""
let squotes d = "'" ^ d ^ "'"
let repeat n d = String.concat "" (List.init n (fun _ -> d))
let concat ds = String.concat "" ds
let separate sep ds = String.concat sep ds
let concat_map f xs = String.concat "" (List.map f xs)
let separate_map sep f xs = String.concat sep (List.map f xs)
let optional f = function None -> "" | Some x -> f x
let flow sep ds = String.concat sep ds
let flow_map sep f xs = String.concat sep (List.map f xs)
let jump _ _ d = d
let softline = " "
let blank n = String.make n ' '
let twice d = d ^ d
let ifflat a _ = a

module OCaml = struct
  let unit = "()"
  let bool b = if b then "true" else "false"
  let int i = string_of_int i
  let int32 i = Int32.to_string i
  let int64 i = Int64.to_string i
  let float f = string_of_float f
  let char c = Printf.sprintf "%C" c
  let string s = Printf.sprintf "%S" s
  let option f = function None -> "None" | Some x -> "Some " ^ f x
  let list f xs = "[" ^ String.concat "; " (List.map f xs) ^ "]"
  let array f xs = "[|" ^ String.concat "; " (Array.to_list (Array.map f xs)) ^ "|]"
  let ref f r = "ref " ^ f !r
  let tuple ds = "(" ^ String.concat ", " ds ^ ")"
  let record _ fields =
    "{ " ^ String.concat "; " (List.map (fun (k, v) -> k ^ " = " ^ v) fields) ^ " }"
  let variant _ tag _ ds =
    if ds = [] then tag else tag ^ " (" ^ String.concat ", " ds ^ ")"
  let flowing_list f xs = list f xs
  let flowing_array f xs = array f xs
end

module ToFormatter = struct
  type channel = Format.formatter
  let pretty _ _ channel d = Format.pp_print_string channel d
  let compact channel d = Format.pp_print_string channel d
end

module ToChannel = struct
  type channel = out_channel
  let pretty _ _ channel d = output_string channel d
  let compact channel d = output_string channel d
end

module ToBuffer = struct
  type channel = Buffer.t
  let pretty _ _ b d = Buffer.add_string b d
  let compact b d = Buffer.add_string b d
end
