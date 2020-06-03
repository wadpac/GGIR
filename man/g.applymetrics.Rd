\name{g.applymetrics}
\alias{g.applymetrics}
\title{
  Extract metrics from acceleration signals
}
\description{
  Function to extract metrics from acceleration signal. Not intended
  for direct use by user
}
\usage{
  g.applymetrics(data,n,sf,ws3,metrics2do,lb=0.2, hb=15)
}
\arguments{
  \item{data}{
    Three column matrix with x, y, and z acceleration data
  }
  \item{n}{
    filter order, only needed if a metric is selected
    that involves a frequency filter
  }
  \item{sf}{
     sample frequency
  }
   \item{ws3}{
     Epoch size in seconds
  }
  \item{metrics2do}{
    Dataframe with Boolean indicator for all metrics whether
    they should be extracted or not. For instance,
    metrics2do$do.bfen = TRUE, indicates that the bfen metric
    should be extracted
  }
  \item{lb}{
    see \link{g.metric}
  }
  \item{hb}{
    see \link{g.metric}
  }
  
}
\value{
Dataframe with metric values in columns average per epoch (ws3)
}

\examples{
  Gx = runif(n=10000,min=0,max=2)
  Gy = runif(n=10000,min=1,max=3)
  Gz = runif(n=10000,min=0,max=2)
  data = cbind(Gx, Gy, Gz)
  metrics2do = data.frame(do.bfen=TRUE,do.enmo=TRUE,do.lfenmo=FALSE,
  do.en=FALSE,do.hfen=FALSE,do.hfenplus=FALSE,do.mad=FALSE,do.anglex=FALSE,
  do.angley=FALSE,do.anglez=FALSE,do.roll_med_acc_x=FALSE,
  do.roll_med_acc_y=FALSE,do.roll_med_acc_z=FALSE,
  do.dev_roll_med_acc_x=FALSE,do.dev_roll_med_acc_y=FALSE,
  do.dev_roll_med_acc_z=FALSE,do.enmoa=FALSE,
  do.lfx=FALSE, do.lfy=FALSE, do.lfz=FALSE, 
                          do.hfx=FALSE, do.hfy=FALSE, do.hfz=FALSE, 
                          do.bfx=FALSE, do.bfy=FALSE, do.bfz=FALSE)
  extractedmetrics = g.applymetrics(data,n=4,sf=40,ws3=5,metrics2do)
}

\author{
Vincent T van Hees <vincentvanhees@gmail.com>
}
