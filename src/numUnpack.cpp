#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerMatrix numUnpack(IntegerVector pack) {
  // Get length
  int n = pack.size();
  // Create array for output
  IntegerMatrix out(n, 3);
  int value, x, y, z, shift = 0;
  // Main loop
  for (int i=0; i<n ;i++){
    // eezzzzzz zzzzyyyy yyyyyyxx xxxxxxxx
    value = pack(i);
  
    // Isolate each axis component
    x = value & 0x03ff;
    y = (value >> 10) & 0x03ff;
    z = (value >> 20) & 0x03ff;
  
    // Signed
    if (x >= 0x200)
      x = x - 0x400;
    if (y >= 0x200)
      y = y - 0x400;
    if (z >= 0x200)
      z = z - 0x400;
  
    shift = (value >> 30) & 0x03;
    x = x << shift;
    y = y << shift;
    z = z << shift;
  
    out(i, 0) = x;
    out(i, 1) = y;
    out(i, 2) = z;
  }
  return out;
}