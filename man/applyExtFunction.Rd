\name{applyExtFunction}
\alias{applyExtFunction}
\title{
  Apply external function to acceleration data.
}
\description{
  Applies external function to the raw acceleration data within GGIR.
  This makes it easier for new algorithms developed externally to be pilotted
  on accelerometer data while taking advantage of the existing comprehensive GGIR
  data management and analysis infrastructure.
  This function is not for direct interaction by user, please supply object 
  \code{myfun} to \link{g.shell.GGIR} or \link{g.part1}. Object \code{myfun}
  is a list as detailed below.
}
\usage{
  applyExtFunction(data, myfun, sf, ws3)
  
}
\arguments{
  \item{data}{
    Data data.frame as present internally in \link{g.getmeta}. It has at least
    four columns of which the first is the timestamp followed by the x, y,
    and z acceleration.
  }
  \item{myfun}{
    See details, in short: myfun is a list object that holds the external function
    to be applied to the data and various parameters to aid in the process.
  }
  \item{sf}{
    Sample frequency (Hertz) of the data object) 
  }
  \item{ws3}{
    Short epoch size (first value of windowsizes in \link{g.getmeta}).
  }
}
\value{
  The output of the external algorithm aggregated or repeated to fit the
  short epoch length of GGIR. Therefore, the short epoch length of GGIR
  should be a multitude of the resolution of the external function output,
  or the visa versa.
}
\details{
    List object, in the form: list(FUN=exampleExtFunction,
              parameters = c(0.2, -0.4), 
              expected_sample_rate= 30,
              expected_unit="mg",
              colnames = c("A","B","C"),
              minlength = 5,
              outputres = 1,
              outputtype="numeric",
              aggfunction = sum)
    \itemize{
    \item \code{FUN} the external function to be applied to the data.
    \item \code{parameters} The parameters used by the function, which 
    can be stored in any format (vector, matrix, list, data.frame). 
    The user should make sure that the external
    function can handle these.
    \item \code{expected_unit} Expected unit of the acceleration by 
    external function: "mg", "g" or "ms2".
    \item \code{colnames} Character vector with the names of the
    columns produced by the external function.
    \item \code{minlength} The minimum length (seconds) of data needed, 
    typically the window per which output is provided.
    \item \code{outputres} The resolution (seconds) of the output 
    produced by the external function. Note, that this needs to be
    equal to or a multitude of the short epoch size of the g.part1 output
    (5 seconds) or the short epoch size should be a multitude of this
    resolution. In this way GGIR can aggregate or repeate the external
    function output to be used inside GGIR.
    \item \code{outputtype} Type of external function output. Set to
    "numeric" if data is stored in numbers (any format).
    \item \code{aggfunction} The function used for aggregation if the
    data needs to be aggregated to match the short epoch size of the
    g.part1 output (5 seconds).
    }
}
\author{
  Vincent T van Hees <vincentvanhees@gmail.com>
}