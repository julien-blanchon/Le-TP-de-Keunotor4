let rec n_a_zero n = match n with
| 0 -> [0]
| _ -> n::(n_a_zero (n-1))


let zero_a_n n = 
  let rec zero_a_n_term n i = 
  if i=n then [n]
  else i::(zero_a_n_term n (i+1))
  in zero_a_n_term n 0

let indice_de l e =
  let rec indice_de_rec l e i =
    match l with 
    | [] -> []
    | queue::tete ->
      if queue=e then i::(indice_de_rec tete e (i+1))
      else indice_de_rec tete e (i+1)
    in indice_de_rec l e 0