type input =
  (* print a viewable item *)
  | View of string
  (* spin the wheel (ie. move forward) *)
  | Spin
  (* give player/bank x amt of money *)
  | Pay of string * int 
  (* take x amt of money from player/bank *)
  | Take of string * int
  (* borrow x amt of money from bank*)
  | Borrow of int
  (* return x amt of money to bank*)
  | Repay of int
  (* buy home/car insurance *)
  | Insure of string
  (* buy stocks *)
  | Invest of int
  (* print instructions *)
  | Help 
  (* relinquish control to next player *)
  | Done
  (* save the game *)
  | Save
  (* quit game *)
  | Exit

exception Bad_input

let rec clean lst = 
  match lst with
  | [] -> []
  | h::t -> let htrim = String.trim h in
    if htrim = "" then clean t else htrim::(clean t)

let parse str : input =
  let str_list = clean (String.split_on_char ' ' str) in
  let length = List.length str_list in 
  if length = 0 then raise Bad_input
  else
    (* head *)
    let action = String.lowercase_ascii (List.hd str_list) in 
    (* object phrase *)
    let target = List.tl str_list in
    match action with 
    | "view" -> if length <> 2 then raise Bad_input else
        View(List.hd target)
    | "spin" -> if length <> 1 then raise Bad_input else
        Spin 
    | "pay" -> if length <> 3 then raise Bad_input else
        begin
          try (
            let player = String.lowercase_ascii (List.hd target) in 
            let amt = int_of_string(List.hd (List.tl target)) in 
            Pay(player, amt)
          )
          with Failure(x) -> raise Bad_input
        end
    | "take" -> if length <> 3 then raise Bad_input else
        begin
          try (
            let player = String.lowercase_ascii (List.hd target) in 
            let amt = int_of_string(List.hd (List.tl target)) in 
            Take(player, amt)
          )
          with Failure(x) -> raise Bad_input
        end
    | "help" -> if length <> 1 then raise Bad_input else Help 
    | "done" -> if length <> 1 then raise Bad_input else Done
    | "save" -> if length <> 1 then raise Bad_input else Save
    | "exit" -> if length <> 1 then raise Bad_input else Exit
    | "borrow" -> if length <> 2 then raise Bad_input else
        begin
          try (let num_loans = int_of_string (List.hd target) in 
               Borrow(num_loans)
              )
          with Failure(x) -> raise Bad_input
        end
    | "repay" -> if length <> 2 then raise Bad_input else
        begin
          try (let num_ret = int_of_string (List.hd target) in 
               Repay(num_ret)
              )
          with Failure(x) -> raise Bad_input
        end
    | "insure" -> if length <> 2 then raise Bad_input else
        Insure(List.hd target)
    | "invest" -> if length <> 2 then raise Bad_input else
        begin 
          try (let num_stock = int_of_string (List.hd target)
               in Invest(num_stock))
          with Failure(x) -> raise Bad_input
        end
    | _ -> raise Bad_input