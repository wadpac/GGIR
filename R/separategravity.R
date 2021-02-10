separategravity = function(acc, gyr, sf) {
  # Copyright: Vincent van Hees, Accelting, v.vanhees@accelting.com
  # gyr: 3-column matrix of gyroscope values in radians per second
  # acc: 3-column matrix of acceleration values in g-units
  # sf: sample frequency
  N = nrow(gyr) # number of time points
  gyr = as.matrix(gyr)
  #====================================================================
  # convert gyroscope to angular velocity vector with
  # an orientation vector (OV) per sample relative to 
  # local coordinate system of the sensor
  # and a magnitude of angular velocity around that vector (theta)
  theta = sqrt(rowSums(gyr ^ 2)) # magnitude of theta (angular velocity)
  OV = matrix(0, N, 3)
  nozero = which(theta > 0)
  OV[nozero,] = as.numeric(gyr[nozero,] / theta[nozero])
  rm(gyr)
  # divide theta by sample frequency
  # because raw data is expressed in units per second, while we need change per sample
  theta = theta / sf
  #====================================================================
  # Derive weights to decide when to rely on accelerometer or gyroscope
  #====================================================================
  # Split low- and high-frequency component
  lb = 0.5 # cut-off frequency for the filter in Hertz
  lowpf = signal::butter(n=4,c(lb/(sf/2)),type=c("low")) #creating filter coefficients
  acc_lf = acc_hf = matrix(NA, nrow(acc), ncol(acc)) # initialize matrices
  for (i in 1:3) {
    # note: acc_lf will also be used as assume orientation of gravity in the
    # absence of movement further down, so this calculate serves two purposes
    acc_lf[,i] <- signal::filter(lowpf, acc[,i]) # low-pass filtered
  }
  acc_hf <- acc - acc_lf # high-pass filtered
  #----------------------------------------------------------------
  # Turning absolute sum of acceleration into weights.
  # Any value below 0.04 will get weight=0.
  # The role is that it provides a minor
  # slope between weight for summed acceleration 0.04 (weight = 0) and 
  # weight for summed acceleration 0.05 (weight=1).
  weight = pmin(pmax((rowSums(abs(acc_hf)) - 0.04),0) / 0.01, 1) 
  rm(acc_hf)
  #----------------------------------------------------------------
  # Maximum weight value. By setting the maximum weight value to 1-(1/sf),
  # we ensure that there is always negative exponential shift from gyroscope based
  # orientation to accelerometer based orientation. This to counteract a
  # possible drift in the gyroscope derived orientation, and assuming that even under
  # dynamic circumstances the low-pass filtered accelerometer can provide a
  # crude estimate of the average orientation of the sensor across multiple seconds.
  maxweight = 1-(0.5/sf)
  #----------------------------------------------------------------
  weight = ifelse(weight > maxweight, yes = maxweight, no = weight)
  # Minimum non-zero weight value. By setting a minimum non-zero weight value to 0.01,
  # we ensure that when the gyro contribution is small it is ignored. This then speeds
  # up the algorithm, because the vector rotation does not have to be applied
  weight = ifelse(weight < 0.01, yes = 0, no = weight)
  #====================================================================
  # Fuse the acc and gyr signal:
  # acc_lf is the low-pass filter acceleration signals, and therefore 
  # our best estimate of the orientation of gravity within the local coordinate
  # system of sensor when the sensor is not moving.
  # In the following lines I calculate values gvector,
  # which is initialized as equal to acc_lf but rotated with the gyroscope information
  # relative to orientation of gravity in the preceding timestep if there is movement.
  # Now I will use theta and OV to rotate acc_lf, but proportional to amount of rotation

  # This equation is standard geometric computation
  # https://stackoverflow.com/questions/6721544/circular-rotation-around-an-arbitrary-axis
  RotArr = array(dim = c(N, 3, 3)) # this is a rotation matrix for every timestep
  costheta = cos(theta)
  sintheta = sin(theta)
  RotArr[,1,1:3] = cbind(costheta +
                           OV[,1]^2 * (1-costheta), OV[,1] * OV[,2] * (1- costheta) -
                           OV[,3] * sintheta, OV[,1] * OV[,3] * (1- costheta) +
                           OV[,2] * sintheta)
  RotArr[,2,1:3] = cbind(OV[,2] * OV[,1] * (1- costheta) +
                           OV[,3] * sintheta, costheta +
                           OV[,2]^2 * (1-costheta),OV[,2] * OV[,3] * (1- costheta) -
                           OV[,1] * sintheta)
  RotArr[,3,1:3] = cbind(OV[,3] * OV[,1] * (1- costheta) -
                           OV[,2] * sintheta, OV[,3] * OV[,2] * (1- costheta) +
                           OV[,1] * sintheta,  costheta +
                           OV[,3]^2 * (1-costheta))
  rm(OV)
  gvector = acc_lf # initialize gvector as equivalent of acc_lf
  weight_not_zero = which(weight > 0)
  if (weight_not_zero[1] == 1) weight_not_zero = weight_not_zero[2:length(weight_not_zero)]
  for (j in weight_not_zero) {
    # Note that this is an iterative process: Each step depends on previous step,
    # which makes it difficult to speed this up
    gvector[j,] = (crossprod(RotArr[j-1,,],  gvector[j-1,]) * weight[j]) + (acc_lf[j,] * (1-weight[j]))
  }
  acclocal = acc - gvector
  invisible(list(acclocal=acclocal, gvector=gvector))
}