#' Split-apply-combine on a level
#'
#' @export
sac_level <- function(by = NULL, ...) {
  do_internal(N = NULL, by = by, ..., FUN = sac_level_internal, from="sac_level")
}





#' @importFrom rlang quo_text eval_tidy
sac_level_internal <- function(N = NULL,
                              ID_label = NULL,
                              working_environment_ = NULL,
                              by = NULL,
                              data_arguments = NULL) {

    uu <- attr(working_environment_, "active_df")
    df <- working_environment_[[uu]]


    idx <- split(seq_len(nrow(df)), df[by], drop = TRUE)


    for(slice in idx) {
      wenv <- import_data_list(df[slice, ,drop=FALSE])

      wenv <- modify_level_internal(N, ID_label, wenv, data_arguments)

      ret <- active_df(wenv)

      # If new columns were created, preallocate them, ow will be ignored w/ a warning
      df[setdiff(names(ret), names(df))] <- NA

      df[slice, ] <- ret

    }

    working_environment_[[uu]] <- df

    working_environment_
}
