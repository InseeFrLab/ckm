#' Global variables declaration
#'
#' This file declares global variables used throughout the package to avoid
#' R CMD check warnings about undefined global variables when using non-standard evaluation.
#'
#' @import utils
#' @name globals
#' @keywords internal
#' @noRd
utils::globalVariables(
  c("ck_end", "v","res_ckm","p_int_ub","p_int_lb","i","nb_obs","n","rkey",
    "p", "X", "Xp", "ckey", "rkey_tot", ".",
    "A", "B", "Ap", "Bp", "R", "Ro", "Rp", "Ao", "Bo", "zA", "zB", "pb",
    "p_zA", "p_zB", "Delta_R", "p_Ap_Bp", "Sup_beta", "p_ecart",
    "p_delta_sup_beta", "p_delta_sup_beta", "proba",
    "nb_obs_ckm","quantile",
    "quant","val","HRC", "TAB", "VAR", "XYZXYZ")
)
