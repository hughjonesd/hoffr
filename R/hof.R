
merge.list <- function (x, y, ...) {
  if (length(x) == 0)
    return(y)
  if (length(y) == 0)
    return(x)
  i <- match(names(y), names(x))
  i <- is.na(i)
  if (any(i))
    x[names(y)[which(i)]] = y[which(i)]
  x
}

# takes a call and a 'list' of args
add_args <- function(call, args) {
  l <- as.list(call)
  orig_args <- l[-1]
  args <- merge.list(args, orig_args)
  as.call(c(l[[1]], args))
}


call_args <- function(call) as.list(call[-1])


clean_env <- function (fn, fn_call, shell, without_defaults) {
  e <- new.env(parent = parent.frame(2))
  assign("fn", fn, envir = e)
  assign("fn_call", fn_call, e)
  assign("shell", shell, envir = e)
  assign("without_defaults", without_defaults, e)
  e
}

#' Create a higher order function
#'
#' @param fn The original function.
#'
#' @return A function with class 'hof', wrapping the original.
#' @export
#'
#' @examples
#'
hof <- function(fn) {
  # a pairlist. Empty args are of type "symbol", but so are
  # defaults
  # like e.g. a = x; for a = sum(1:3), typeof is "language".
  args <- formals(fn)
  arg_names <- names(args) # can include "..."
  has_defaults <- sapply(args, function (arg) arg != quote(expr = ))
  without_defaults <- arg_names[! has_defaults]
  with_defaults    <- arg_names[has_defaults]
  without_defaults <- without_defaults[! without_defaults == "..."]
  fn_call <- match.call(fn, call('fn'))
  # auto fills in default arguments:
  fn_call <- add_args(fn_call, args[with_defaults])

  shell <- function (...) {
    given_args <- list(...) # we evaluate given args eagerly
    fn_call_new <- hoffr:::add_args(fn_call, given_args)
    # this gives an error if inappropriate arguments have been given:
    fn_call_new <- match.call(fn, fn_call_new, expand.dots = TRUE)
    if (
      # the call must have been given named arguments either as defaults
      # or previously; unnamed arguments are totally optional
      # as.list...[-1] gets the arguments
      all(without_defaults %in% names(as.list(fn_call_new[-1])))
    ) {
      return(eval(fn_call_new))
    } else {
      shell_copy <- shell
      environment(shell_copy) <- hoffr:::clean_env(fn, fn_call_new, shell,
            without_defaults)
      return(shell_copy)
    }
  }

  environment(shell) <- clean_env(fn, fn_call, shell, without_defaults)
  class(shell) <- c('hof', class(fn))
  shell
}



#' Print a hof object
#'
#' @param x An object of class hof.
#' @param ... Unused.
#' @return NULL invisibly.
#' @export
#'
#' @examples
#' lm_hof <- hof(lm)
#' iris_lm <- lm_hof(data = iris)
#' iris_lm
print.hof <- function(x, ...) {
  cat("Higher order function:\n")
  fn <- get("fn", envir = environment(x))
  print(fn)
  cat("Arguments:")
  cl <- get("fn_call", envir = environment(x))
  cl[[1]] <- substitute(fn)
  print(cl)
  invisible(NULL)
}

# ideas:
# hold(fnl, ... ) allows you to fill in arguments without calling the function,
#                 even if all arguments are complete:
#   ready_to_call <- hold(tmp)(a = 1, b = 2)
#   ready_to_call()
# clear(fnl, ...) sets listed arguments (or all) to missing again:
#   tmpa1 <- tmp(a = 1)
#   same_as_tmp <- clear(tmpa1)
# args_needed(fnl) reports what is missing before the argument can be called
# args_provided(fnl) reports what arguments have already been entered
# add_args could perhaps be made a public interface, cf. partial
#
# functional.default could look like
#   function (..., .body)
# where ... provides the function's arguments
# functional.function could just call
#   functional.default(formals(f), .body = f)
# perhaps with some Rlang splicing magic
# problem, how to set a literal ... argument? You can't call a function with ...
# So maybe have a .dots = n argument where n gives the argument position, -1
# means last?

# things to consider.
# 1. Should it be OK to do:
#   tmp2 <- tmp(a=x)
#   x <- 200
#   tmp2(b=10)
# i.e. should arguments be evaluated lazily?
# But that seems likely to create code errors, and in general, why not wait
# before calling? So, NO.
#
# 2. If you do
#   foo <- functional(function (a=b, d) sum(a, d))
# does that mean, evaluate b right now and set it as the default?
# my sense would be not, because if you want to include a specific argument
# that varies, you would actually just do
#   foo <- functional(function(a, d) sum(a, d))
#   b <- 1:10
#   foo(a = b, d = 20)
# Whereas, you might want to do e.g.
#   foo <- functional(function(y, x = seq_along(y)) plot(x, y))
#
# 3. I want to be able to do:
#   foo <- functional(function(a, b) a+b)
#   add1 <- foo(a=1)
#   add1(b = 2)
#   add1(b = 3)
# But, I also want to be able to do:
#   foo(a = 1, b = 2)
#   foo(a = 1, b = 3)
# without complaining about overwritten arguments. And:
#   foo(a = 1, b = 2)
#   add1 <- foo(a = 1) # without this now just giving 3
#   add1(b = 3)
# On the other hand, shouldn't the following give an error?
#   add1 <- foo(a = 1)
#   add1(a = 2, b = 3) # oops! overwrote an argument.
# Maybe not; maybe we just always overwrite arguments
#
# 4. What if I want e.g. to make sprintf functional?
# Formals of sprintf look like: function (fmt, ...)
# But, if called as sprintf("%s") it complains, too few arguments.
# I'd like to run
#   as_string <- sprintf_functional("%s")
# without triggering the function.
# Could I do
#   as_string <- sprintf_functional("%s", ...)
# ?
# No, you can never use ... in a function call, only a definition.
# Perhaps "hold()" would be useful here.


