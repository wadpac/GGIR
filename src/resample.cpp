#include <Rcpp.h>
#include <cmath>
#include <cfenv>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix resample(NumericMatrix raw, NumericVector rawTime,
                       NumericVector time, int stop, int type=1) {
  // raw is stop-by-N matrix with raw values of x, y, z, and optionally additional columns.
  // rawTime is vector with stop elements of raw time.
  // time is array with required time points.
  // type is type of interpolation (1 = linear, 2 = nearest neighbour)
  // get number of columns
  int Ncols = raw.ncol();
  // Calculate the last resamplable point in time
  int last = 0, nTime = time.size();
  double u = rawTime(stop - 1);
  for (; (last<nTime) && (time(last) <= u); last++);
  // Create output array
  NumericMatrix res(last,Ncols);
  switch (type) {
    case 1: { // linear
      for (int j = 0; j < Ncols; j++) { //columns
        int pos = 1; // It is right border of the interval to use    
        for (int p = 0; p < last; p++) { //p is number of point to calculate
          for (; rawTime(pos) < time(p); pos++);
          u = (time(p) - rawTime(pos - 1)) / (rawTime(pos) - rawTime(pos - 1));
          res(p, j) = u * (raw(pos ,j) - raw(pos - 1, j)) + raw(pos-1 ,j);
        }  
      }
    }
    break;
    case 2: { //nearestNeighbour
      for (int j = 0; j < Ncols; j++) { //columns
        int pos = 1; // It is right border of the interval to use    
        for (int p = 0; p < last; p++) { //p is number of point to calculate
          for (; rawTime(pos) < time(p); pos++);
          u = (time(p) - rawTime(pos - 1)) / (rawTime(pos) - rawTime(pos - 1));
          res(p, j) = std::round(u) * (raw(pos ,j) - raw(pos - 1, j)) + raw(pos-1 ,j);
        }  
      }
    }
    break;
  }
  return res;
}
