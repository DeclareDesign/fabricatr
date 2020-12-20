#' @param x
#' @param conditions
#'
#' @importFrom rlang eval_tidy as_quosure f_lhs
#' @export
potential_outcomes <- function(x, conditions = list(Z = c(0, 1)), sep = "_") {
  conds <- expand.grid(conditions, stringsAsFactors = FALSE)
  pos <- list()
  for(i in 1:nrow(conds)) {
    cond <- conds[i, , drop = FALSE]
    nm <- paste0(f_lhs(x), sep, paste0(names(cond), sep, cond, collapse = sep))
    list2env(x = as.list(cond), environment(x))
    pos[[nm]] <- eval_tidy(as_quosure(x))
  }
  as.data.frame(pos)
}


#' @param x
#' @param conditions
#'
#' @importFrom rlang eval_tidy f_lhs as_name
#' @export
reveal_outcomes <- function(x){

  # obtain character strings
  outcome_variables <- as_name(f_lhs(x))
  assignment_variables <- labels(terms(x))
  assignment_variables_expr <- str2lang(paste0("data.frame(", paste0(assignment_variables, collapse = ", "), ")"))

  # obtain assignment columns
  assignment_data <- eval_tidy(assignment_variables_expr, env = environment(x))

  # list of assignmentname_conditionvalue
  potential_cols <-
    mapply(paste,
           assignment_variables,
           assignment_data,
           sep = "_",
           SIMPLIFY = FALSE)

  potential_cols <- do.call(paste, c(outcome_variables, potential_cols, sep = "_"))

  upoc <- unique(potential_cols)

  # obtain potential outcomes columns
  po_variables_expr <- str2lang(paste0("data.frame(", paste0(upoc, collapse = ", "), ")"))
  po_data <- eval_tidy(po_variables_expr, env = environment(x))

  matching_rows <- seq_len(nrow(po_data))
  matching_cols <- match(potential_cols, colnames(po_data))

  as.data.frame(po_data)[cbind(matching_rows, matching_cols)]
}

