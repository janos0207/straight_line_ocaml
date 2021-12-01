type id = string

type binop = Plus | Minus | Times | Div

type stm =
  | CompoundStm of stm * stm
  | AssignStm of id * exp
  | PrintStm of exp list

and exp =
  | IdExp of id
  | NumExp of int
  | OpExp of exp * binop * exp
  | EseqExp of stm * exp

let prog =
  CompoundStm
    ( AssignStm ("a", OpExp (NumExp 5, Plus, NumExp 3)),
      CompoundStm
        ( AssignStm
            ( "b",
              EseqExp
                ( PrintStm [ IdExp "a"; OpExp (IdExp "a", Minus, NumExp 1) ],
                  OpExp (NumExp 10, Times, IdExp "a") ) ),
          PrintStm [ IdExp "b" ] ) )

let max a b = if a > b then a else b

let rec maxargs stm =
  let maxargs_exp exp =
    match exp with EseqExp (stm, _) -> maxargs stm | _ -> 0
  in
  match stm with
  | CompoundStm (smt1, stm2) -> max (maxargs smt1) (maxargs stm2)
  | AssignStm (_, exp) -> maxargs_exp exp
  | PrintStm exps -> List.length exps

let () = print_int (maxargs prog)
