\name{ShellDoc2Vignette}
\alias{ShellDoc2Vignette}
\title{
  Extract Definition from Shell Documentation
}
\description{
  Function extracts the documentation for a given GGIR argument as provided in the \link{GGIR} documentation.
  Function not designed for direct use by package user.
}
\usage{
  ShellDoc2Vignette(argument = \Sexpr{format(paste0('"',formals(GGIR::ShellDoc2Vignette)[["argument"]],'"'))})
}
\arguments{
  \item{argument}{
    Character (default = \Sexpr{format(paste0('"',formals(GGIR::ShellDoc2Vignette)[["argument"]],'"'))}). 
    Name of the argument to extract the definition.
  }
}
\value{
  Character object with the definition of the argument.
}
\author{
  Jairo Hidalgo Migueles <jairo.hidalgo.migueles@gmail.com>
}