creer_matrice_transition <- function(D, V, js, ...){

  p_table <- ptable::create_cnt_ptable(D = D, V = V, ...)

  return(p_table)
}

creer_table_perturbation <- function(matrice_transition){

  require(data.table)

  table_perturbation <- matrice_transition@pTable[, .(i,v,p_int_lb,p_int_ub)]
  data.table::setkey(table_perturbation, i, p_int_lb, p_int_ub)

  return(table_perturbation)
}
