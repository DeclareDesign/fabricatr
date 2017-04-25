

format_num <- function(x, digits = 3) {
  x <- as.numeric(x)
  return(paste0(sprintf(paste0("%.", digits, "f"), x)))
}


#' Describe Variable
#'
#' @param x a vector representing a single variable
#'
#' @return a data.frame summary of the variable
#' @export
#'
#' @examples
#'
#' var1 <- sample(1:5, 50, replace = T)
#' describe_variable(var1)
#'
#' var2 <- runif(50)
#' describe_variable(var2)

#' var3 <- rep(letters[1:5], each = 5)
#' describe_variable(var3)
#'
#' var4 <- rep(c(TRUE, FALSE), c(70, 30))
#' describe_variable(var4)
#'
#'
describe_variable <- function(x){

  num_unique <- length(unique(x))
  num_missing <- sum(is.na(x))

  if (num_unique <= 5) {

    tab <- table(x, exclude = NULL)
    prop_tab <- prop.table(tab)

    df <-
      cbind(
        data.frame(tab, stringsAsFactors = FALSE)[,2],
        data.frame(prop_tab, stringsAsFactors = FALSE)[,2]
      )

    obj <- data.frame(t(df))
    obj[1,] <- format_num(obj[1,], digits = 0)
    obj[2,] <- format_num(obj[2,], digits = 2)
    obj <- cbind(c("Frequency", "Proportion"), obj)
    colnames(obj) <- c("", names(tab))

  } else if (typeof(x) %in% c("character", "factor") & num_unique > 5) {

    obj <- data.frame(N_missing = num_missing,
                      N_unique = num_unique, stringsAsFactors = FALSE)

    } else {
    obj <- data.frame(min = min(x, na.rm = TRUE),
                      median = median(x, na.rm = TRUE),
                      mean = mean(x, na.rm = TRUE),
                      max = max(x, na.rm = TRUE),
                      sd = sd(x, na.rm = TRUE),
                      N_missing = num_missing,
                      N_unique = num_unique, stringsAsFactors = FALSE)
    obj[, c("min", "median", "mean", "max", "sd")] <-
      apply(obj[, c("min", "median", "mean", "max", "sd")], 2, format_num, digits = 2)
  }

  rownames(obj) <- NULL
  return(obj)
}


