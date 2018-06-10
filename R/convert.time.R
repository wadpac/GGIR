# GGIR used to depend on CRAN package GENEAread as developed by Joss Langford and Zhou Fang.
# Now GENEAread is depricated the essential parts of the code has been copied to GGIR to ensure
# ongoing functionality.

#extends time display from package chron to use h:m:s for >1 day times.

as.GRtime <- function(x, format = NULL, ...){
  if (is.character(x)){
    return(convert.time(parse.time(x, ...), format))
  } else { 
    #try to coerce
    return( convert.time(as.numeric(x, ...), format))
    
  }
}


c.GRtime <-
  function (..., recursive = FALSE) 
    structure(c(unlist(lapply(list(...), unclass))), class = "GRtime")

#Somewhat hackish - we fix image.default by having Axis attempt disambiguation on xlim and ylim if classed
#Axis <-
#function (x = NULL, at = NULL, ..., side, labels = NULL) 
#{
#    if (!is.null(x)) 
#        UseMethod("Axis", x)
#    else if (!is.null(at)) 
#        UseMethod("Axis", at)
#    else axis(side = side, at = at, labels = labels, ...)
#}




convert.time = function(x, format = NULL){
  #require(chron)
  #units = match.arg(units)
  ##convert to days
  #if (units == "seconds") x = x /(60*60*24)
  
  ##this bit might trigger a y2k style bug, remove when time formats are rationalised or we go to a class based system
  #if (x[1] < (946684800/(60*60*24))) x = x + 946684800/(60*60*24)
  #out = times(x,...)
  if (!inherits(x, "GRtime")) class(x) = c("GRtime",class(x))
  attr(x, "format") = format
  x
}

Summary.GRtime <- 
  function(x,..., na.rm)
  {
    ok <- switch(.Generic, max = , min = , range = TRUE, FALSE)
    att = attributes(x)
    if (!ok)
      stop(.Generic, " not defined for GRtime objects")
    val <- NextMethod(.Generic)
    attr(val, "format") = att$format
    class(val) <- att$class #]oldClass(list(...)[[1L]])
    val
  }
Ops.GRtime <-
  function (e1, e2) 
  {
    if (nargs() == 1) 
      stop("unary ", .Generic, " not defined for GRtime objects")
    boolean <- switch(.Generic, `<` = , `>` = , `==` = , `!=` = , 
                      `<=` = , `>=` = TRUE, FALSE)
    if (boolean) {
      #        stop(.Generic, " not defined for Date objects")
      if (is.character(e1)) 
        e1 <- parse.time(e1)
      if (is.character(e2)) 
        e2 <- parse.time(e2)
      return(NextMethod(.Generic))
    } 
    
    arith <- switch(.Generic, `+` = , `-` = , `*` = , `/` = , `%/%`=
                      ,  `%%` = TRUE, FALSE)
    unitpres <- switch(.Generic, `+` = , `-` = ,  `%%` = TRUE, FALSE)
    
    if (inherits(e1, "GRtime")){
      att = attributes(e1)
      if ((!unitpres) && inherits(e2, "GRtime")) att = NULL
    } else {
      att = attributes(e2)
    }
    if (!arith) stop(.Generic, " not defined for GRtime objects")
    
    val = NextMethod(.Generic)
    attributes(val) = att
    return(val)
    
    
  }



`[.GRtime` <-
  function (x, ..., drop = TRUE) 
  {
    cl <- oldClass(x)
    class(x) <- NULL
    val <- NextMethod("[")
    class(val) <- cl
    val
  }



print.GRtime <-
  function(x, quote = FALSE, format, ...)
  {
    if(!as.logical(length(x))) {
      cat("GRtimes(0)\n")
      return(invisible(x))
    }
    if (missing(format)) format = NULL
    #    if(missing(simplify) &&
    #       is.null(simplify <- getOption("chron.simplify")))
    #        simplify <- FALSE
    xo <- x
    ## print whole days (no fraction) as regular integers
    #ZF#    if(all(is.na(x)) || any(x[!is.na(x)] >= 1))
    #ZF#       cat("Time in days:\n")
    x <- format.GRtime(x, format = format)
    #    NextMethod("print", quote = quote)
    print(x, quote = quote)
    invisible(xo)
  }


"format.GRtime"<-
  function(x, format = NULL,...)#format. = "h:m:s", simplify = FALSE, ...)
  { 
    #	x = x /(60*60*24)
    if(!as.logical(length(x)))
      return("")
    if(all(is.na(x)))
      return(rep("NA", length = length(x)))
    if(!is.numeric(x))
      stop(paste(deparse(substitute(x)), "must be numeric"))
    att <- attributes(x)
    if (is.null(format)) format = att$format #maybe x has a preset format?
    if (is.null(format)){
      #choose a smart format depending on the time range?
      #most things, or single times: h:m
      #short interval < 10 minutes h:m:s
      #supershort < 10 seconds m:s:ms
      #long >24 hours d h:m
      #very long >  72 hours
      format = "%H:%M"
      if (length(x) > 1){
        rng = diff(range(x, na.rm = TRUE))
        if (rng < 10){
          format = "%M:%OS3"
        } else if (rng < 10*60) {
          format = "%H:%M:%S"
        } else if (rng < 24*60*60){
          format = "%H:%M"
        } else if (rng < 7*60*60*24){
          format = "%a %H:%M"
        } else {
          format = "%d/%m %H:%M"
        }
      }
    }
    
    #    if(inherits(x, "times")) {
    #       if(missing(format.))
    #          format. <- switch(mode(att$format),
    #                           character = ,
    #                          list = rev(att$format)[[1]],
    #                         name = ,
    #                        "function" = att$format,
    #                       NULL = format.,
    #                      stop("invalid output times format"))
    #        class(x) <- NULL
    #   }
    #    if(!is.character(format.)) {
    #       ## format may be a function or name
    #      FUN <- switch(mode(format.),
    #                   "function" = format.,
    #                  name = eval(format.),
    #                 stop(paste("unrecognized time format",
    #                           deparse(substitute(format.)))))
    # return(FUN(unclass(x), ...))
    #    }
    #   else format. <- rev(format.)[1]	
    #  nas <- is.na(x)
    #  
    att$class <- att$format <- NULL
    ## <NOTE>
    ## DJ's design is that
    ##   times greater than 1 day  should format like numerics
    ## To change this (e.g., have times(1.5) format as 36:00:00), simply
    ## comment the code below, and make the corresponding change in
    ## print.times().
    out = format(as.POSIXct(as.numeric(x), origin = "1970-1-1", tz = "UTC"), format, ...)
    
    #    days <- abs(floor(x))
    #   if(any(days[!nas] > 0)) {
    #	x = x - floor(x)
    #   attributes(x) <- att
    #   return(format(x))
    #    }
    ## </NOTE>
    #    sec <- round(24 * 3600 * abs(x))
    #   hh <- sec %/% 3600
    #  mm <- (sec - hh * 3600) %/% 60
    # ss <- round(sec - hh * 3600 - 60 * mm) # round instead of truncate
    #out <- list(h = substring(paste("0", hh, sep = ""), nchar(paste(hh))), 
    #		m = substring(paste("0", mm, sep = ""), nchar(paste(mm))),
    #               s = substring(paste("0", ss, sep = ""), nchar(paste(ss))))
    #  style <- parse.format(format.)
    #override style
    #style = list(periods = c("h", "m", "s"), sep = ":")
    #   o <- style$periods
    #  if(!simplify)
    #     out <- paste(out[[o[1]]], out[[o[2]]], out[[o[3]]],
    #                 sep = style$sep)
    #    else {
    #       if(simplify == 1) {
    #          ## no secs
    #         o <- o[o != "s"]
    #        out <- paste(out[[o[1]]], out[[o[2]]], sep = style$sep)
    #   }
    #  else out <- out$h
    #    }
    #   if(any(x[!nas] < 0))
    #      out <- paste(ifelse(x < 0, "-", " "), out, sep = "")
    # out[nas] <- "NA"
    out[x == Inf] <- "Inf"
    out[x ==  - Inf] <- "-Inf"
    attributes(out) <- att
    out
  }

#"plot.times2" <-
#function(x, y, ...,
#         xlab = deparse(substitute(x)), ylab = deparse(substitute(y)),
#         simplify)
#{
##    if(missing(simplify))
# #       if(is.null(simplify <- getOption("chron.simplify")))
#  #          simplify <- TRUE
#   # x.times <- inherits(x, "times")	# is x a times?
#    if(missing(y)) {
#        x <- sort(x)                    # NA's will be ignored
#        y <- seq_along(as.vector(x))
#        if(missing(ylab))
#            ylab <- "Counts"
#    }
#    y.times <- inherits(y, "times")	# is y a times?
#    dots <- list(...)
#    if(is.null(axes <- dots$axes)) axes <- TRUE # do we draw axes? 
#    ## only xaxt="n" or yaxt="n" requests in ... are honored!
#    if(is.null(req.xaxt <- dots$xaxt) || req.xaxt != "n")
#        req.xaxt <- "s"
#    if(is.null(req.yaxt <- dots$yaxt) || req.yaxt != "n")
#        req.yaxt <- "s"
#    old <- par("xaxt", "yaxt")
#    on.exit(par(old))
#    ## trap graphical pars in ... that affect axis() in addition to plot()
#    if(is.null(adj <- dots$adj))
#        adj <- par("adj")
#    if(is.null(cex <- dots$cex.axis))
#        cex <- par("cex")
#    if(is.null(col <- dots$col.axis))
#        col <- par("col")
#    if(is.null(font <- dots$font.axis))
#        font <- par("font")
#    if(is.null(las <- dots$las))
#        las <- par("las")
#    if(is.null(lab <- dots$lab))
#        lab <- par("lab")
#    if(is.null(mgp <- dots$mgp))
#        mgp <- par("mgp")
#    if(is.null(tcl <- dots$tcl)) tcl <- par("tcl")	
#    ## for some plot types we need to sort according to x
#    if(!is.null(type <- dots$type))
#        if(any(type == c("l", "b", "o"))) {
#            xlab; ylab                  # force promises
#            nas <- is.na(x)
#            o <- order(x[!nas])
#            x <- x[!nas][o]
#            y <- y[!nas][o]
#        }
#    xx <- unclass(x)
#    yy <- unclass(y)
#    if(x.times)
#        xaxt <- "n"
#    else xaxt <- req.xaxt
#    if(y.times)
#        yaxt <- "n"
#    else yaxt <- req.yaxt
#    if(!is.null(l <- dots$log)) {
#        if(inherits(x, "dates") && any(l == c("x", "xy", "yx")))
#            stop("cannot do logarithmic plot of a dates object")
#        if(inherits(y, "dates") && any(l == c("y", "xy", "yx")))
#            stop("cannot do logarithmic plot of a chron object")
#    }
#    ## unfortunately we can't use (easily) NextMethod when y is missing!
#    plot.default(xx, yy, xlab = xlab, ylab = ylab, ...,
#                 xaxt = xaxt, yaxt = yaxt)
#    if(axes) {
#        if(req.xaxt == "n")
#            par(xaxt = "n")
#        else if(x.times)
#            axis.times2(1, x, simplify = simplify, labels = TRUE,
#                       adj = adj, col = col, cex = cex, font = font,
#                       las = las, lab = lab, mgp = mgp, tcl = tcl)
#        if(req.yaxt == "n")
#            par(yaxt = "n")
#        else if(y.times)
#            axis.times2(2, y, simplify = simplify, srt = 90, labels
#                       = TRUE, adj = adj, col = col, cex = cex,
#                       font = font, las = las, lab = lab, mgp = mgp,
#                       tcl = tcl)
#    }
#    invisible(list(x = x, y = y))
#}
#

Axis.GRtime <- function(x = NULL, at = NULL, ..., side, labels = TRUE){
  axis.GRtime(side = side, x = x, at = at, labels = labels, ...)
}

pretty.GRtime <- function(x, n = 5, ...) {
  att = attributes(x)
  attributes(x) = NULL
  x = as.numeric(pretty(as.POSIXct(x, origin = "1970-1-1", tz = "UTC"), n,...))
  attributes(x) = att
  x
}
"axis.GRtime"<-
  function(side, x=NULL, at=NULL, format = NULL,labels  = TRUE, add = TRUE,  ...)
  {
    if (is.null(at)){
      #	    if(!inherits(x, "GRtime")) x <- convert.time(x)
      att <- NULL
      if (inherits(x, "GRtime")) att <- attributes(x)
      bad <- (is.na(x) | abs(as.vector(x)) == Inf)
      if(side == 1 || side == 3){
        rng =  par("usr")[1:2]
        n = par("xaxp")[3] +1
      } else {
        rng = par("usr")[3:4]
        n = par("yaxp")[3] +1
      }
      tmp <- c(rng, as.numeric(x[!bad]))
      attributes(tmp) = att
      at = pretty.GRtime(tmp, n)
    } else {
      if(!inherits(at, "GRtime")) at <- convert.time(at)
    }
    if(missing(labels) || (is.logical(labels) && labels)) 
      labels <- format(at, format = format)
    if(add)
      axis(side, at = at, labels = labels, ...)
    invisible(list(side = side, at = at, labels = labels))
  }

