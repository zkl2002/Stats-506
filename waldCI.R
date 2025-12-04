.wald_bounds <- function(m, se, level) {
  if (length(m) != 1 || length(se) != 1 || !is.finite(m) || !is.finite(se)) {
    stop("mean and sterr must be finite length-1 numbers")
  }
  if (se <= 0) stop("sterr (standard error) must be positive")
  if (length(level) != 1 || !is.finite(level) || level <= 0 || level >= 1) {
    stop("level must be a single number in (0,1)")
  }
  m  <- unname(m)
  se <- unname(se)
  level <- unname(level)
  z  <- qnorm(1 - (1 - level)/2)
  c(lb = m - z*se, ub = m + z*se)
}

.wald_from_bounds <- function(lb, ub, level) {
  if (length(lb) != 1 || length(ub) != 1 || !is.finite(lb) || !is.finite(ub))
    stop("lb and ub must be finite length-1 numbers")
  if (lb > ub) stop("lb cannot be greater than ub")
  if (length(level) != 1 || !is.finite(level) || level <= 0 || level >= 1)
    stop("level must be a single number in (0,1)")
  lb <- unname(lb)
  ub <- unname(ub)
  level <- unname(level)
  m  <- (lb + ub)/2
  half <- (ub - lb)/2
  z  <- qnorm(1 - (1 - level)/2)
  se <- half / z
  c(mean = m, sterr = se)
}

setClass(
  "waldCI",
  slots = c(
    mean  = "numeric",
    sterr = "numeric",
    level = "numeric"
  )
)

setValidity("waldCI", function(object) {
  if (length(object@mean)  != 1 || !is.finite(object@mean))
    return("mean must be a single finite number")
  if (length(object@sterr) != 1 || !is.finite(object@sterr))
    return("sterr must be a single finite number")
  if (object@sterr < 0)
    return("sterr cannot be negative")
  if (length(object@level) != 1 || !is.finite(object@level) ||
      object@level <= 0 || object@level >= 1)
    return("level must be a single number in (0,1)")
  TRUE
})

waldCI <- function(level, mean = NULL, sterr = NULL, lb = NULL, ub = NULL) {
  if (missing(level)) stop("Must supply 'level' in (0,1)")
  if ( (!is.null(mean) || !is.null(sterr)) && (!is.null(lb) || !is.null(ub)) )
    stop("Supply either (mean, sterr) or (lb, ub), not both")
  
  if (!is.null(mean) || !is.null(sterr)) {
    if (is.null(mean) || is.null(sterr))
      stop("If mean or sterr is supplied, BOTH must be supplied")
    obj <- new("waldCI",
               mean  = as.numeric(mean),
               sterr = as.numeric(sterr),
               level = as.numeric(level))
    validObject(obj); return(obj)
  }
  
  if (!is.null(lb) || !is.null(ub)) {
    if (is.null(lb) || is.null(ub))
      stop("If lb or ub is supplied, BOTH must be supplied")
    par <- .wald_from_bounds(as.numeric(lb), as.numeric(ub), as.numeric(level))
    obj <- new("waldCI",
               mean  = par["mean"],
               sterr = par["sterr"],
               level = as.numeric(level))
    validObject(obj); return(obj)
  }
  
  stop("Supply (level, mean, sterr) OR (level, lb, ub)")
}


setGeneric("show", function(object, level = 0.95, digits = 4){
  standardGeneric("show")
}
)

setMethod("show", "waldCI", function(object) {
  b <- .wald_bounds(object@mean, object@sterr, object@level)
  cat("waldCI:\n",
      "  level = ", format(object@level), "\n",
      "  CI    = (", format(b["lb"]), ", ", format(b["ub"]), ")\n",
      "  mean  = ", format(object@mean), "\n",
      "  sterr = ", format(object@sterr), "\n", sep = "")
  invisible(object)
})


setGeneric("lb", function(x) standardGeneric("lb"))
setMethod ("lb", "waldCI", 
           function(x) .wald_bounds(x@mean, x@sterr, x@level)["lb"])

setGeneric("ub", function(x) standardGeneric("ub"))
setMethod ("ub", "waldCI", 
           function(x) .wald_bounds(x@mean, x@sterr, x@level)["ub"])

setGeneric("level", function(x) standardGeneric("level"))
setMethod ("level", "waldCI", function(x) x@level)

# mean()
setMethod("mean", "waldCI", function(x, ...) x@mean)

# sterr()
setGeneric("sterr", function(x) standardGeneric("sterr"))
setMethod("sterr", "waldCI", function(x) x@sterr)


setGeneric("lb<-", function(x, value) standardGeneric("lb<-"))
setMethod ("lb<-", "waldCI", function(x, value) {
  other <- ub(x)
  par <- .wald_from_bounds(as.numeric(value), other, x@level)
  x@mean <- par["mean"]
  x@sterr <- par["sterr"]
  validObject(x)
  x
})

setGeneric("ub<-", function(x, value) standardGeneric("ub<-"))
setMethod ("ub<-", "waldCI", function(x, value) {
  other <- lb(x)
  par <- .wald_from_bounds(other, as.numeric(value), x@level)
  x@mean <- par["mean"]
  x@sterr <- par["sterr"]
  validObject(x)
  x
})

setGeneric("mean<-", function(x, value) standardGeneric("mean<-"))
setMethod ("mean<-", "waldCI", function(x, value) {
  x@mean <- as.numeric(value)
  validObject(x)
  x
})

setGeneric("sterr<-", function(x, value) standardGeneric("sterr<-"))
setMethod ("sterr<-", "waldCI", function(x, value) {
  x@sterr <- as.numeric(value)
  validObject(x)
  x
})

setGeneric("level<-", function(x, value) standardGeneric("level<-"))
setMethod ("level<-", "waldCI", function(x, value) {
  x@level <- as.numeric(value)
  validObject(x)
  x
})


setGeneric("contains", function(x, value) standardGeneric("contains"))

setMethod ("contains", "waldCI", function(x, value) {
  b <- .wald_bounds(x@mean, x@sterr, x@level)
  as.numeric(value) >= b["lb"] && as.numeric(value) <= b["ub"]
})


setGeneric("overlap", function(x, y) standardGeneric("overlap"))

setMethod ("overlap", c("waldCI","waldCI"), function(x, y) {
  b1 <- .wald_bounds(x@mean, x@sterr, x@level)
  b2 <- .wald_bounds(y@mean, y@sterr, y@level)
  max(b1["lb"], b2["lb"]) <= min(b1["ub"], b2["ub"])
})


setMethod("as.numeric", "waldCI", function(x, ...) {
  .wald_bounds(x@mean, x@sterr, x@level)[c("lb","ub")]
})


transformCI <- function(x, fun) {
  stopifnot(is(x, "waldCI"), is.function(fun))
  message("Note: only monotonic functions make sense for transforming CIs.")
  lo <- lb(x)
  hi <- ub(x)
  tlo <- fun(lo)
  thi <- fun(hi)
  new_lb <- min(tlo, thi)
  new_ub <- max(tlo, thi)
  waldCI(level = x@level, lb = new_lb, ub = new_ub)
}