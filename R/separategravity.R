separategravity = function(acc, gyr, sf) {
  # Copyright: Vincent van Hees, Accelting, v.vanhees@accelting.com
  
  # gyr: 3-column matrix of gyroscope values in radians per second
  # acc: 3-column matrix of acceleration values in g-units
  # sf: sample frequency
  N = nrow(gyr) # number of time points

  #====================================================================
  # convert gyroscope to angular velocity vector with
  # an orientation vector (omega_ori) per sample relative to 
  # local coordinate system of the sensor
  # and a magnitude of angular velocity around that vector (theta)
  theta = sqrt(rowSums(gyr ^ 2)) # magnitude of omega (angular velocity)
  getorientation = function(S) { 
    if (S[4] >0) {
      tmp = as.numeric(S[1:3]/S[4]) # normalise vector
    } else {
      tmp = matrix(0, 1, 3)
    }
    return(tmp)
  }
  omega_ori = t(apply(cbind(gyr, theta), FUN = getorientation, 1))
  # divide theta by sample frequency
  # because raw data is expressed in units per second, while we need change per sample
  theta = theta / sf
  #====================================================================
  # Derive weights to decide when to rely on accelerometer or gyroscope
  #====================================================================
  # Split low- and high-frequency component
  lb = 0.5
  lowpf = signal::butter(n=4,c(lb/(sf/2)),type=c("low")) #creating filter coefficients
  acc_lf = acc_hf = matrix(NA, nrow(acc), ncol(acc)) # initialize matrices
  for (i in 1:3) {
    # note: acc_lf will also be used as assume orientation of gravity in the 
    # absence of movement further down, so this calculate serves two purposes
    acc_lf[,i] <- signal::filter(lowpf, acc[,i]) # low-pass filtered
    acc_hf[,i] <- acc[,i] - acc_lf[,i] # high-pass filtered
  }
  # initialize weight vector: weight = 1 rely on gyroscope (default)
  weight = rep(1, nrow(acc))
  # update weight vector (rely on accelerometer (weight=0) when
  # summed absolute acceleration over 3 axis <= 0.04
  weight = pmin(pmax((rowSums(abs(acc_hf)) - 0.04),0) / 0.02, 1) 
  # smooth the weight values with the same low-pass filter
  weight = filter(lowpf, weight)
  # trim off weights above 1 and below 0.01
  weight = ifelse(weight > 1, yes = 1, no = weight)
  weight = ifelse(weight < 0.01, yes = 0, no = weight)
  
  #====================================================================
  # Fuse the acc and gyr signal:
  # acc_lf is the low-pass filter acceleration signals, and therefore 
  # our best estimate of the orientation of gravity within the local coordinate
  # system of sensor when the sensor is not moving.
  # In the following lines I calculate values gvector,
  # which is initialized as equal to acc_lf but rotated with the gyroscope information
  # relative to orientation of gravity in the preceding timestep if there is movement.
  # Now I will use theta and omega_ori to rotate acc_lf, but proportional to amount of rotation
  ux = omega_ori[,1]
  uy = omega_ori[,2]
  uz = omega_ori[,3]
  rm(omega_ori)
  # This equation is standard geometric computation
  # https://stackoverflow.com/questions/6721544/circular-rotation-around-an-arbitrary-axis
  RotArr = array(dim = c(N, 3, 3)) # this is a rotation matrix for every timestep
  RotArr[,1,1:3] = cbind(cos(theta) +
                           ux^2 * (1-cos(theta)), ux * uy * (1- cos(theta)) -
                           uz * sin(theta), ux * uz * (1- cos(theta)) +
                           uy * sin(theta))
  RotArr[,2,1:3] = cbind(uy * ux * (1- cos(theta)) +
                           uz * sin(theta), cos(theta) +
                           uy^2 * (1-cos(theta)),uy * uz * (1- cos(theta)) -
                           ux * sin(theta))
  RotArr[,3,1:3] = cbind(uz * ux * (1- cos(theta)) -
                           uy * sin(theta), uz * uy * (1- cos(theta)) +
                           ux * sin(theta),  cos(theta) +
                           uz^2 * (1-cos(theta)))
  gvector = acc_lf # initialise gvector as equivalent of acc_lf
  skipped = rep(0, N)
  weight_not_zero = which(weight > 0)
  if (weight_not_zero[1] == 1) weight_not_zero = weight_not_zero[2:length(weight_not_zero)]
  weight_zero = which(weight <= 0)
  if (length(weight_zero) > 0) skipped[weight_zero] = 2
  print(paste0("not zeros: ",length(weight_not_zero)," zeros: ",length(weight_zero)))
  for (j in weight_not_zero) {
    # Note that this is an iterative process: Each step depends on previous step,
    # which makes it difficult to speed this up
    gvector[j,] = (crossprod(RotArr[j-1,,],  gvector[j-1,]) * weight[j]) + (acc_lf[j,] * (1-weight[j]))
  }
  acclocal = acc - gvector
  invisible(list(acclocal=acclocal, gvector=gvector, skipped=skipped, weight=weight, acc_lf=acc_lf))
}