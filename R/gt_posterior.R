#' Format a gt table for posterior summary
#'
#' Format a gt table for posterior summary. Usually from \code{quap},
#' \code{brm} and \code{inla}.
#'
#' The format will rename the quantiles based on the \code{qtl} argument which
#' must have the same number of element as the corresponding columns.
#'
#' @param df Dataframe. Posterior summary.
#' @param var_col Name of the column with the variables.
#' @param labs List with the following items
#' @param qtl List used to change the name of the quantile columns. Should
#' be formatted as, for example.
#' @param .data_color Data color used by \code{gt::data_color()}
#' @param .tab_options List of tab_options with the following defaults
#' \describe{
#'   \item{data}{"mintcream"}
#'   \item{heading.background.color}{"lavender"}
#'   \item{heading.title.font.weight}{"bold"}
#'   \item{heading.subtitle.font.weight}{"bold"}
#'   \item{column_labels.background.color}{"oldlace"}
#'   \item{column_labels.font.weight}{"bold"}
#'   \item{stub.background.color}{"oldlace"}
#'   }
#' @param digits Integer. Digits used to format data numbers.
#'
#' @import dplyr gt
#'
#' @return gt table of posterior summary
#' @export
gt_posterior <- function(df, var_col="var",
                         labs=list(title="Posterior Summary",
                                   subtitle="Practice"),
                         qtl = NULL, .data_color="mintcream",
                         .tab_options = NULL, digits = 2L) {

  # use default tab_options if NULL is given
  .tab_options_default <- list(
    heading.background.color = "lavender",
    heading.title.font.weight = "bold",
    heading.subtitle.font.weight = "bold",
    column_labels.background.color = "oldlace",
    column_labels.font.weight = "bold",
    stub.background.color = "oldlace")
  # add default tab_options when item is missing
  pos <- !(names(.tab_options_default) %in% names(.tab_options))
  .tab_options <- append(x = .tab_options, values = .tab_options_default[pos])


  # create the gt table
  out <- df %>%
    tibble::rownames_to_column(var = var_col) %>%
    gt::gt(rowname_col = var_col) %>%
    tab_header(title = labs$title,
               subtitle = labs$subtitle) %>%
    cols_label(.list = qtl) %>%
    cols_align(align = "center", columns = everything()) %>%
    fmt_number(columns = everything(), decimals = digits) %>%
    data_color(columns = everything(), colors = .data_color)

  # update the tab_options
  .tab_options <- append(x = .tab_options, values = list(data = out))
  do.call(gt::tab_options, .tab_options)
}

#' Create a gt table comparing posteriors
#'
#' Create a gt table comparing posteriors. Usually from \code{quap}, \code{brm}
#' and \code{inla}.
#'
#' The list of summaries must meet these conditions
#' \itemize{
#'   \item The summary called "var_df" in the list will be used to name the rows
#'   \item All summaries must have the rows in the same order
#'   \item All summaries must have the same column names
#'   }
#'
#' @param dfs List of dataframes with posterior summaries.
#' @param var_df Name of the summary to use as a reference for variable names.
#' @param var_col Name of the column with the variables.
#' @param labs List with the following items
#' @param .data_color Data color used by \code{gt::data_color()}
#' @param .tab_options List of tab_options with the following defaults
#' \describe{
#'   \item{"data"}{"mintcream"}
#'   \item{"heading.background.color"}{"darkslategray"}
#'   \item{"heading.title.font.weight"}{"bold"}
#'   \item{"heading.subtitle.font.weight"}{"bold"}
#'   \item{"column_labels.background.color"}{"seashell"}
#'   \item{"column_labels.font.weight"}{"bold"}
#'   \item{"stub.background.color"}{"seashell"}
#'   }
#' @param digits Integer. Digits used to format data numbers.
#'
#' @import dplyr gt
#'
#' @return gt table of posterior summary comparison
#' @export
gt_posterior_compare <- function(dfs, var_df = names(dfs)[1], var_col = "var",
                                 labs = list(title = "Posterior Summary Comparison",
                                             subtitle = "Practice"),
                                 .data_color="mintcream", .tab_options = NULL,
                                 digits = 2L) {
  stopifnot(var_df %in% names(dfs))

  # use default tab_options if NULL is given
  .tab_options_default <- list(
    heading.background.color = "darkslategray",
    heading.title.font.weight = "bold",
    heading.subtitle.font.weight = "bold",
    column_labels.background.color = "seashell",
    column_labels.font.weight = "bold",
    stub.background.color = "seashell")
  # add default tab_options when item is missing
  pos <- !(names(.tab_options_default) %in% names(.tab_options))
  .tab_options <- append(x = .tab_options, values = .tab_options_default[pos])

  # combine the dataframes
  out <- lapply(X = names(dfs), FUN = function(x) {
    # keep the "var" column only for the first dataframe
    if (x != var_df) dfs[[x]] <- dfs[[x]][, names(dfs[[x]]) != var_col]
    # add the name of the method to the variables except for the var_col
    sel <- names(dfs[[x]]) != var_col
    names(dfs[[x]])[sel] <- paste(names(dfs[[x]])[sel], x, sep = "_")
    dfs[[x]]
  })
  out <- bind_cols(out)

  # create the gt table
  out <- out %>%
    gt::gt(rowname_col = "var") %>%
    tab_header(title = labs$title,
               subtitle = labs$subtitle) %>%
    tab_spanner_delim(delim = "_", columns = everything(), split = "first") %>%
    cols_align(align = "center", columns = everything()) %>%
    fmt_number(columns = everything(), decimals = digits) %>%
    data_color(columns = everything(), colors = .data_color)

  # update the tab_options
  .tab_options <- append(x = .tab_options, values = list(data = out))
  do.call(gt::tab_options, .tab_options)
}
