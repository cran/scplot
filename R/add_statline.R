#' Add a statline to an scplot
#'
#' @inheritParams .inherit_scplot
#' @param stat A character string for defining a statistical line or curve to be
#'   plotted.
#' @param phase Either a numeric or a character vector specifying the reference
#'   phase (see details).
#' @param label A character string which is used to set the label in a legend.
#' @param ... additional parameters passed to the statistical function.
#' @details The `phase` argument defines the reference phase for some
#'   statistical functions (`"median", "mean", "min", "max", "quantile"`). The
#'   default is `NULL` which calculates and plots statistics for each phase
#'   separately. The arguments takes a numeric vector (phase number(s)) or a
#'   character vector (phase name(s)). When more than one phase is defines,
#'   statistics are based on the combined values of these phases. Various
#'   methods for an extrapolated *trendA* line exist: `"trendA"` is based on an
#'   OLS regression, `"trendA theil-sen"` on a nonparametric regression, and
#'   `"trendA bisplit"` / `"trendA trisplit"` are two median based approaches.
#'   Some of the functions defined in `stats` have additional arguments. The
#'   [mean()] function has a trim argument (e.g. `trim = 0.1`). [quantile()] has
#'   a proportion argument (e.g. `prob = 0.75` for calculating the 75%
#'   quantile). `moving mean` and `moving median` have a lag argument (e.g. `lag
#'   = 2`). The local-regression curve function `"lowess"` (or `"loreg"`) has a
#'   proportion argument (e.g. `f = 0.5`; see [lowess()]) and the
#'   local-regression curve function `"loess"` has a span argument (e.g. `span =
#'   0.75`; see [loess()]).
#' @return An object of class `scplot` (see[scplot()]) with changed element
#'   `statlines`.
#' @export
add_statline <- function(object,
                         stat = c("mean", "median", "min", "max", "quantile",
                                  "sd", "mad",
                                  "trend", "trendA", "trendA theil-sen",
                                  "trendA bisplit", "trendA trisplit",
                                  "moving mean", "moving median",
                                  "loreg", "lowess", "loess"),
                         phase = NULL,
                         color = NULL,
                         linewidth = NULL,
                         linetype = NULL,
                         variable = NULL,
                         label = NULL,
                         ...) {

  stat <- match.arg(stat)

  if (is.null(variable)) variable <- ".dvar"

  line <- list(
    linewidth = linewidth,
    colour = color,
    linetype = linetype
  )

  n_lines <- length(object$statlines)
  if (n_lines == length(object$theme$statline)) n_lines <- 1
  line <- .merge_element(
    line,
    object$theme$statline[[n_lines + 1]]
  )
  object$theme$statline[[length(object$statlines) + 1]] <- line

  args <- list(...)

  if (identical(stat, "trend") && identical(phase, "A")) {
    phase <- NULL
    stat <- "trendA"
  }

  if (!is.null(args$method)) {
    if (args$method == "theil-sen" && stat == "trendA") {
      stat <- "trendA theil-sen"
      args$method <- NULL
    }
    if (args$method == "bisplit" && stat == "trendA") {
      stat <- "trendA bisplit"
      args$method <- NULL
    }
    if (args$method == "trisplit" && stat == "trendA") {
      stat <- "trendA trisplit"
      args$method <- NULL
    }
  }

  new <- list(
    stat = stat,
    phase = phase,
    args = args,
    #line = line,
    variable = variable,
    label = label
  )

  object$statlines <- c(object$statlines, list(new))
  object

}
