
```
file -> statlist
block -> `{` statlist `}`
statlist -> {stat} [laststat]

stat -> assignment
      | fn_call
      | block
      | WHILE expr block
      | IF expr block {ELSEIF expr block} [ELSE block]
      | FOR names IN expr_list block
      | FN fn_name fn_body

laststat -> return [expr_list] | break

assignment -> vars `=` expr_list
vars -> suffix_expr {`,` suffix_expr}

fn_call -> suffix_expr args
fn_body -> `(` [parlist] `)` block
fn_name -> Name {`.` Name}
args ->  `(` [expr_list] `)`
parlist -> Name {`,` Name} [`,`]
expr_list -> expr {`,` expr}

expr -> literal
      | tableconstructor
      | FN fn_body
      | suffix_expr
      | fn_call
      | expr binop expr
      | unop expr

primary_expr -> Name | '(' expr ')'
literal -> nil | Bool | Numeral | String
suffix_expr -> prefix { suffix }
prefix -> primary_expr | fn_call
suffix -> `.` Name
        | `[` expr `]`

tableconstructor -> `{` [fieldlist] `}`
fieldlist -> field {`,` field}
field -> expr | Name `=` expr
```
