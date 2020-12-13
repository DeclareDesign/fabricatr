
#' @param x
#' @param conditions
#'
#' @importFrom rlang eval_tidy
#' @importFrom dplyr bind_cols
#' @export
draw_potential_outcomes <- function(x, conditions) {
  conds <- expand.grid(conditions, stringsAsFactors = FALSE)
  pos <- list()
  for(i in 1:nrow(conds)) {
    cond <- conds[i, , drop = FALSE]
    nm <- paste0("_", paste0(names(cond), "_", cond, collapse = "_"))
    list2env(x = as.list(cond), environment(x))
    pos[[nm]] <- eval_tidy(as_quosure(x), env = environment(x))
  }
  bind_cols(pos)
}
