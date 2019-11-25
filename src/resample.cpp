#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix resample(NumericMatrix raw, NumericVector rawTime, NumericVector time, int stop) {
  // raw is stop-by-N matrix with raw values of x, y, z, and optionally additional columns.
  // rawTime is vector with stop elements of raw time.
  // time is array with required time points.
  // stop is the number of the last known point in raw and rawTime
  // get number of columns
  int Ncols = raw.ncol();
  // Calculate the last resamplable point in time
  int last = 0, nTime = time.size();
  double u = rawTime(stop - 1);
  for (; (last<nTime) && (time(last) <= u); last++);
  // Create output array
  NumericMatrix res(last,Ncols);
//  NumericMatrix res(5,3);
  //Main loop
  int pos = 1; // It is right border of the interval to use
  for (int p = 0; p < last; p++){ //p is number of point to calculate
    for (; rawTime(pos) < time(p); pos++);
    // Calculate value which is constant for this resampling
    u = (time(p) - rawTime(pos - 1)) / (rawTime(pos) - rawTime(pos - 1));
    for (int j = 0; j < Ncols; j++)
      res(p, j) = u * (raw(pos ,j) - raw(pos - 1, j)) + raw(pos-1 ,j);
  }

  return res;
}
