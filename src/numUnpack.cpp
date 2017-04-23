#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerMatrix numUnpack(IntegerVector pack) {
  // Get length
  int n = pack.size();
  // Create array for output
  IntegerMatrix out(n, 3);
  // Main loop
  int mask = 0x0ffc0;
  int tmp, work, expon = 0;
  for (int i=0; i<n ;i++){
    tmp = pack(i);
    expon = 6 - ((tmp>>30) & 0x03);
    work = ((tmp <<  6) & mask) >> expon;
    if (work > 2048)
      work -= 4096;
    out(i, 0) = work;
    work = ((tmp >>  4) & mask) >> expon;
    if (work > 2048)
      work -= 4096;
    out(i, 1) = work;
    work = ((tmp >> 14) & mask) >> expon;
    if (work > 2048)
      work -= 4096;
    out(i, 2) = work;
  }
  return out;
}
