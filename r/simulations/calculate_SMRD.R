calculate_SMRD <- function(data){
  colnames(data)[colnames(data) == "ID"] <- "partid"
  # Error checks
  if(any(c("partid", "r_risk_binary", "aspect_ind") %in% colnames(data) == FALSE)) {
    stop("Data must contain columns r_risk, partid, and aspect_ind")
  }
  
  # Calculate how many different kind of aspects (i.e., seeking and avoiding) 
  # have been listed
  both_types <- data[, sum(any(r_risk_binary == "seeking") != 0,
                           any(r_risk_binary == "avoiding") != 0), by = partid]
  
  # SMRD when both aspect types have been listed
  SMRD1 <- data[partid %in% both_types[V1 == 2, partid], 
                .(SMRD = 2 * (median(aspect_ind[r_risk_binary == "avoiding"]) -
                                median(aspect_ind[r_risk_binary == "seeking"])) /
                    length(aspect_ind)), by = partid]
  
  # SMRD when none aspect type has been listed
  SMRD2 <- data[partid %in% both_types[V1 == 0, partid], .(SMRD = 0), by = partid]
  
  # SMRD when only one aspect type have been listed
  SMRD3 <- data[partid %in% both_types[V1 == 1, partid], 
                .(SMRD = if (any(r_risk_binary == "seeking") == 0) {
                  2 * (median( aspect_ind[r_risk_binary == "avoiding"] ) -
                         (length(aspect_ind) + 1 )) / (length(aspect_ind) + 1)
                } else {
                  2 * (length(aspect_ind) + 1 -
                         median(aspect_ind[r_risk_binary == "seeking"])) /
                    (length(aspect_ind) + 1) 
                }), 
                by = partid]
  
  # Combine SMRDs
  SMRD <- rbind(SMRD1, SMRD2, SMRD3)
  return(SMRD)
}
