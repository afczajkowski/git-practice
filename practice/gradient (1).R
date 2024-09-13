gradient <- function(sal,time,depth){
  
  # Calculate time and depth differences
  time_diff <- diff(time)  # Length will be 18
  depth_diff <- diff(depth)  # Length will be 104
  
  # Initialize arrays for gradients
  dSdt <- matrix(0, nrow = nrow(sal), ncol = ncol(sal))    # Gradient with respect to time (105x19)
  dSdz_2 <- matrix(0, nrow = nrow(sal), ncol = ncol(sal))  # Gradient with respect to depth (105x19)
  
  # 1. Gradient with respect to time (columns)
  # Forward difference for the first column
  dSdt[, 1] <- (sal[, 2] - sal[, 1]) / time_diff[1]
  
  # Backward difference for the last column
  dSdt[, ncol(sal)] <- (sal[, ncol(sal)] - sal[, ncol(sal) - 1]) / time_diff[length(time_diff)]
  
  # Central difference for the interior columns
  for (j in 2:(ncol(sal) - 1)) {
    dSdt[, j] <- (sal[, j + 1] - sal[, j - 1]) / (time_diff[j] + time_diff[j - 1])
  }
  
  # 2. Gradient with respect to depth (rows)
  # Forward difference for the first row
  dSdz_2[1, ] <- (sal[2, ] - sal[1, ]) / depth_diff[1]
  
  # Backward difference for the last row
  dSdz_2[nrow(sal), ] <- (sal[nrow(sal), ] - sal[nrow(sal) - 1, ]) / depth_diff[length(depth_diff)]
  
  # Central difference for the interior rows
  for (i in 2:(nrow(sal) - 1)) {
    dSdz_2[i, ] <- (sal[i + 1, ] - sal[i - 1, ]) / (depth_diff[i] + depth_diff[i - 1])
  }
  
  return(dSdz_2)
}