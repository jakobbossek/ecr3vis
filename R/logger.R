#' @title Initialize logger.
#'
#' @description Generates a fairly flexible container to log stuff.
#'
#' @param what [\code{named(character)}]\cr
#'   Named character vector. The names (i.e. \code{names(what)}) describe which
#'   elements we want to log and the actual values represent the types/modes these
#'   values are stored as. Possible values are \dQuote{integer}, \dQuote{numeric},
#'   \dQuote{character}, \dQuote{logical} for the respective \strong{scalar}
#'   atomic values or \dQuote{list} for any other complex R objects, e.g., matrices,
#'   lists etc.
#'   Note that internally \code{c("iter" = "numeric")} is attached automatically
#'   and \code{iter} must be passed to \code{|link{update_logger}} in subsequent
#'   calls.
#' @param init.size [\code{integer(1)}]\cr
#'   Initial size of the log.
#' @param at [\code{integer}]\cr
#'   Optional integer vector of times when logging should take place.
#'   If the iteration counter is in \code{at}, the state is logged, otherwise it
#'   is not.
#' @param to [\code{character(1)}]\cr
#'   File path to log-directory.
#'   If not \code{NULL}, each call to \code{update_logger} stores the serialized
#'   state in file \dQuote{<to>/<iteration>.rds}.
#' @return [\code{enviroment}] Logger environment.
#' @examples
#' log = init_logger(c("P" = "list", "f" = "list", "C" = "character"), init.size = 5L, at = c(1, 3))
#'
#' # note that div is not subject to logging since it was not specified in init_logger
#' update_logger(log, P = matrix(runif(10), ncol = 2L), C = "a", f = runif(10), iter = 1, div = letters[1:3])
#' update_logger(log, P = matrix(runif(10), ncol = 2L), C = "b", f = runif(10), iter = 2, div = letters[1:3])
#' update_logger(log, P = matrix(runif(10), ncol = 2L), C = "a", f = runif(10), iter = 3, div = letters[1:3])
#' print(log$df)
#' @seealso update_logger
#' @export
init_logger = function(what, init.size, at = NULL, to = NULL) {
  checkmate::assert_character(names(what), min.len = 1L, min.chars = 1L, any.missing = FALSE, all.missing = FALSE)
  checkmate::assert_subset(what, choices = c("integer", "numeric", "character", "list"))
  checkmate::assert_integerish(at, lower = 0L, any.missing = FALSE, all.missing = FALSE, null.ok = TRUE)
  !(is.null(to)) && checkmate::test_path_for_output(to, overwrite = TRUE)

  if ("iter" %in% names(what))
    re::stopf("[evoprob::init_logger] Name 'iter' must not be used as a name in list 'what' since it is appended automatically.")

  log = new.env()
  # NOTE: iteration must be logged in any case (mention in description)
  log$what = c(c("iter" = "numeric"), what)
  log$what.names = names(log$what)
  log$size = init.size

  # init data.frame
  # 1) build expression (way easier than the explicit way)
  # 2) parse and evaluate expression
  df.expr = paste0(names(log$what), " = ", unname(log$what), "(", log$size, ")")
  df.expr = gsub("list\\(.*\\)", sprintf("I(replicate(%i, NULL))", log$size), df.expr)
  df.expr = paste0("data.frame(", re::collapse(df.expr, sep = ", "), ", stringsAsFactors = FALSE)")
  log$df = eval(parse(text = df.expr))
  log$curline = 1L

  log$at = at
  log$to = to
  log$log.to.file = !is.null(to)

  class(log) = "evoprob_logger"

  return(log)
}

#' @title Update logger.
#'
#' @description Store stuff in logger object.
#'
#' @param log [\code{enviroment}]\cr
#'   Logger initialized with \code{\link{init_logger}}.
#' @param log.stuff [\code{list}]\cr
#'   Named list of objects.
#'   The subset specified by argument names of parameter \code{what} of \code{\link{init_logger}}
#'   will be stored.
#' @param ... [any]\cr
#'   Variable argument list as another possibility to pass objects.
#'   These arguments are merged with \code{log.stuff}. Note that the dot-args
#'   have precdence, i.e., objects in \code{log.stuff} are owerwritten by objects
#'   named equally in the dot-args list.
#' @return Nothing. The function modifies \code{log} in-place.
#' @seealso init_logger
#' @export
update_logger = function(log, log.stuff = list(), ...) {
  log.stuff = re::insert(log.stuff, list(...))
  log.names = names(log.stuff)

  # sanity checks
  if (!re::is.subset(log$what.names, log.names))
    re::stopf("[evoprob::update_logger] Logger is ought to log fields '%s', but no matching arguments were passed to update_logger.",
      re::collapse(setdiff(log$what.names, log.names), sep = ", "))

  should_log = function(iter) {
    is.null(log$at) | (iter %in% c(log$at))
  }

  iter = log.stuff$iter

  if (log$curline == log$size)
    re::stopf("[evoprob::update_logger] Logger overflow! Resizing not yet implemented.")

  # reduce to wanted stuff
  log.stuff = log.stuff[log$what.names]

  # log either to file or to data.frame
  if (should_log(iter)) {
    if (log$log.to.file) {
      fn = file.path(log$to, paste0(iter, ".rds"))
      saveRDS(log.stuff, file = fn)
    } else {
      # log$df[log$curline, log$what.names] = log.stuff
      for (what.name in log$what.names) {
        if (log$what[what.name] == "list") {
          log$df[[what.name]][[log$curline]] = log.stuff[[what.name]]
        } else {
          # here the only difference is in the access of the line ([[...]] vs [...])
          log$df[[what.name]][log$curline] = log.stuff[[what.name]]
        }
      }
      log$curline = log$curline + 1L
    }
  }
}
