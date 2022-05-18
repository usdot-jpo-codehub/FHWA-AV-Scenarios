


#' Interpolate Values
#' 
#' Interpolates a value in between two different values
#' @param min_value The baseline value to use in the interpolation
#' @param max_value The higher value to use in the interpolation
#' @param min_weight The baseline weight, in this case it is the smaller of the two candiate roadway capacity values
#' @param max_weight The higher weight, in this case it is the higher of the two candiate roadway capacity values
#' @param target_weight The target weight that we are interpolating for, in this case it is our target roadway capacity value
#' @return A numeric value that represents the target value
CalculateInterpolatedValue <- function(min_value, max_value, min_weight, max_weight, target_weight){
  if(is.na(min_weight) | is.na(min_value)){
    if(is.na(max_weight) | is.na(max_value)){
      target_value <- NA
    } else {
      target_value <- max_value
    }
  } else if(is.na(max_weight) | is.na(max_value)){
    target_value <- min_value
  } else if(target_weight == min_weight){
    target_value <- min_value
  } else if(target_weight == max_weight){
    target_value <- max_value
  } else if(is.na(max_weight)){
    target_value <- min_value
  } else if(is.na(max_value)){
    target_value <- min_value
  } else if(is.na(min_value)){
    target_value <- NA
  } else if (max_weight == min_weight){
    target_value = min_value
  } else {
    weight_range <- max_weight - min_weight
    growth <- max_value - min_value
    growth_per_weight <- growth/weight_range
    weight_increment <- target_weight - min_weight
    increment_growth <- weight_increment*growth_per_weight
    target_value <- min_value + increment_growth
  }
  return(target_value)
}


#' Prepare Scenario Specific Inputs
#' 
#' Makes the scenario-specific input list based on the users choice of a scenario
#' @param scenario The specific scenarion that the user wants to run
#' @param df The data.frame that contains all of the scenarion details
#' @param template_list The template list to update with the scenario specific file locations
#' @return A list that contains all of the inputs required to calculate the capacity reduction facotrs
PrepareInputList <- function(scenario, df, template_list){
  # Read file path/locations from the scenario manager table and select the values from the specific scenario
  market_penetration = df[df[, 1] == scenario, 3]
  roadway_system = df[df[, 1] == scenario, 4]
  vehicle_dynamics = df[df[, 1] == scenario, 5]
  caf_lookup = df[df[, 1] == scenario, 6]
  zovcaf_lookup = df[df[, 1] == scenario, 7]
  output = df[df[, 1] == scenario, 8]
  
  # Update the list with the files from the scenario manager table
  template_list$AVMarketPenetration$file <- market_penetration
  template_list$RoadwaySystem$file <- roadway_system
  template_list$VehicleDynamics$file <- vehicle_dynamics
  template_list$CAFLookup$file <- caf_lookup
  template_list$ZOVCAFLookup$file <- zovcaf_lookup
  template_list$output$file <- output
  
  
  return(template_list)
}

#' Roadway Capacity Adjustments
#' 
#' Modifies the roadway capacity of a specific facility based on the percentage of AVs in the fleet, the connectivity of the infrastrucutre, and the driver dynamics associated
#' with the AV fleet. This function uses two different lookup tables to determine the capacity adjustment factor that should be applied to the baseline roadway capacity
#' @param L A list containing the relatve paths to the input files
#' @param write_output A boolean that indicates if the output file should be written, default is TRUE
#' @return A data frame that provides a summary of the Roadway Capacity Adjustements by Facility Type
#' @import dplyr
CalculateRoadwayCapacity <- function(L, write_output=TRUE){
  
  # Read in the Differnt CSV Files Stored in the List
  df_market_penetration <- read.csv(L$AVMarketPenetration$file, stringsAsFactors = FALSE)
  df_roadway_system <- read.csv(L$RoadwaySystem$file, stringsAsFactors = FALSE)
  df_vehicle_dynamics_lookup <- read.csv(L$VehicleDynamics$file, stringsAsFactors = FALSE)
  df_lookup <- read.csv(L$CAFLookup$file, stringsAsFactors = FALSE)
  output <- L$output$file
  
  colnames(df_market_penetration) <- L$AVMarketPenetration$column_names
  colnames(df_roadway_system) <- L$RoadwaySystem$column_names
  colnames(df_vehicle_dynamics_lookup) <- L$VehicleDynamics$column_names
  colnames(df_lookup) <- L$CAFLookup$column_names
  
  # Set up the intermediate data.frame
  df_intermediate_df <- df_roadway_system
  
  # Determine the percentage of fleet that is autonomus (level 4 + level 5)
  df_intermediate_df$AV_percentage <- df_market_penetration[5,2] + df_market_penetration[6,2]
  
  
  # Multiply the minimum value of the global %AV and the AV Allowed for each facility type by the %Connectivity for each facility type.
  cav_percentage <- ((pmin(df_intermediate_df$AV_percentage/100, df_intermediate_df[, L$RoadwaySystem$column_names[4]]/100) * (df_intermediate_df[, L$RoadwaySystem$column_names[3]]/100)))*100
  cav_percentage <- round(cav_percentage, -1)
  df_intermediate_df <- transform(df_intermediate_df, cav_percentage = cav_percentage)
  
  # Add the % AV and % CAV to the Vehicle Dynamics Table
  df_intermediate_df <- dplyr::left_join(df_intermediate_df, df_vehicle_dynamics_lookup, by = L$VehicleDynamics$column_names[1], copy = FALSE, suffix = c(".x", ".y"))
  
  # Use the joined table to look up the CAF
  df_intermediate_lookup<- dplyr::full_join(df_intermediate_df, df_lookup, by = c("facility" = "facility"
                                                                                  , "intervehicle_gap" = "intervehicle_gap"
                                                                                  , "max_platoon_size" = "max_platoon_size"
                                                                                  , "cav_interplatoon_gap" = "cav_interplatoon_gap"
                                                                                  , "cav_percentage" = "cav_percentage"))
  df_intermediate_lookup <- dplyr::filter(df_intermediate_lookup, !is.na(base_capacity.y))
  # Now lets find the values that we want to interpolate for
  df_intermediate_df$caf <- rep(NA, nrow(df_intermediate_df))
  for(r in 1:nrow(df_intermediate_df)){
    row <- df_intermediate_df[r, ]
    message(sprintf("Processing Row %s out of %s", r, nrow(df_intermediate_df)))
    for(c in 1:ncol(row)){
      message(sprintf("|- %s: %s", colnames(row)[c], row[1, c]))
    }
    subset.df <- dplyr::filter(df_intermediate_lookup, facility == row[, L$RoadwaySystem$column_names[1]] & intervehicle_gap == row[, L$CAFLookup$column_names[3]] & max_platoon_size == row[, L$CAFLookup$column_names[4]] & cav_interplatoon_gap == row[, L$CAFLookup$column_names[5]] & cav_percentage == row[, L$CAFLookup$column_names[1]])
    if(nrow(subset.df) == 0){
      message("Could not find a match in the CAF Lookup Table")
      caf <- NA
    } else if(nrow(subset.df) == 1){
      caf <- subset.df[, L$CAFLookup$column_names[7]]
      message(sprintf("Single match on CAF: %s", caf))
    } else {
      input_capacities <- subset.df[, paste0(L$RoadwaySystem$column_names[2],".y")]
      source_capacities <- row[, L$CAFLookup$column_names[6]]
      message(sprintf("Input Capacities: %s", input_capacities))
      message(sprintf("Source Capacities: %s", source_capacities))
      subset.df$difference <- input_capacities - source_capacities
      # Difference values - is there an exact match?
      if(0 %in% subset.df$difference){
        caf <- subset.df$caf[subset.df$difference == 0]
        print(sprintf("Perfect Match on Capacities, CAF: %s", caf))
      } else {
        message("Processing multiple options")
        postive_differences <- subset.df$difference[subset.df$difference > 0]
        negative_differences <- subset.df$difference[subset.df$difference < 0]
        if(length(negative_differences) > 0) {
          min_negative_difference <- max(subset.df$difference[subset.df$difference < 0], na.rm=TRUE)
          min_index <- which(subset.df$difference == min_negative_difference)
          min_weight <- subset.df[min_index, paste0(L$CAFLookup$column_names[6], ".y")]
          min_value <- subset.df[min_index, L$CAFLookup$column_names[7]]
        } else {
          min_weight <- NA
          min_value <- NA
        }
        
        if (length(postive_differences) > 0){
          min_postive_difference <- min(subset.df$difference[subset.df$difference > 0], na.rm=TRUE)
          max_index <- which(subset.df$difference == min_postive_difference)
          max_weight <- subset.df[max_index, paste0(L$CAFLookup$column_names[6], ".y")]
          max_value <- subset.df[max_index, L$CAFLookup$column_names[7]]
        } else {
          max_weight <- NA
          max_value <- NA
        }
        
        
        target_Weight <- row[, L$CAFLookup$column_names[6]]
        message("CHECK INTERPOLATION INPUTS")
        message(sprintf("|- Min Value: %s", min_value))
        message(sprintf("|- Max Value: %s", max_value))
        message(sprintf("|- Min Weight: %s", min_weight))
        message(sprintf("|- Max Weight: %s", max_weight))
        message(sprintf("|- Target Weight: %s", target_Weight))
        caf <- CalculateInterpolatedValue(min_value, max_value, min_weight, max_weight, target_Weight)
        
      }
    }
    if(is.na(caf)){
      df_intermediate_df$caf[r] <- NA
    } else {
      df_intermediate_df$caf[r] <- caf
    }
    
    
  }
  
  
  # Clean up the Table
  message("DF Intermediate DF")
  for(cname in colnames(df_intermediate_df)){
    message(cname)
  }
  message("OUPUT LOOKUPS")
  for(cname in L$output$column_names[1:4]){
    message(cname)
  }
  
  df_outlook <- df_intermediate_df %>% select(L$output$column_names[1:4])
  
  # Apply the CAF to the Baseline Capacity
  df_outlook$adjusted_capacity <- df_outlook[, L$output$column_names[2]] * df_outlook[, L$output$column_names[4]]
  
  # Clean up the column names
  colnames(df_outlook) <- L$output$column_description
  if(write_output){
    message(sprintf("WRITING OUTPUT FILE: %s", output))
    write.csv(df_outlook, output)
    
  }
  
  return(df_outlook)
  
}


#' Roadway Capacity and Vehicle Size Adjustments
#' 
#' Modifies the roadway capacity of a specific facility based on the percentage of AVs in the fleet, the connectivity of the infrastrucutre, the driver dynamics associated
#' with the AV fleet, and vehicle size. This function uses two different lookup tables to determine the capacity adjustment factor that should be applied to the baseline roadway capacity
#' @param L A list containing the relatve paths to the input files
#' @param write_output A boolean that indicates if the output file should be written, default is TRUE
#' @return A data frame that provides a summary of the Roadway Capacity Adjustements by Facility Type
#' @import dplyr
CalculateRoadwayCapacityPCE <- function(L, write_output=TRUE){
  
  # Read in the Differnt CSV Files Stored in the List
  df_market_penetration <- read.csv(L$AVMarketPenetration$file, stringsAsFactors = FALSE)
  df_roadway_system <- read.csv(L$RoadwaySystem$file, stringsAsFactors = FALSE)
  df_vehicle_dynamics_lookup <- read.csv(L$VehicleDynamics$file, stringsAsFactors = FALSE)
  df_lookup <- read.csv(L$CAFLookup$file, stringsAsFactors = FALSE)
  output <- L$output$file
  
  colnames(df_market_penetration) <- L$AVMarketPenetration$column_names
  colnames(df_roadway_system) <- L$RoadwaySystem$column_names
  colnames(df_vehicle_dynamics_lookup) <- L$VehicleDynamics$column_names
  colnames(df_lookup) <- L$CAFLookup$column_names
  
  # Set up the intermediate data.frame
  df_intermediate_df <- df_roadway_system
  
  # Determine the percentage of fleet that is autonomus (level 4 + level 5)
  df_intermediate_df$AV_percentage <- df_market_penetration[5,2] + df_market_penetration[6,2]
  
  
  # Multiply the minimum value of the global %AV and the AV Allowed for each facility type by the %Connectivity for each facility type.
  cav_percentage <- ((pmin(df_intermediate_df$AV_percentage/100, df_intermediate_df[, L$RoadwaySystem$column_names[4]]/100) * (df_intermediate_df[, L$RoadwaySystem$column_names[3]]/100)))*100
  cav_percentage <- round(cav_percentage, -1)
  df_intermediate_df <- transform(df_intermediate_df, cav_percentage = cav_percentage)
  
  # Add the % AV and % CAV to the Vehicle Dynamics Table
  df_intermediate_df <- dplyr::left_join(df_intermediate_df, df_vehicle_dynamics_lookup, by = L$VehicleDynamics$column_names[1], copy = FALSE, suffix = c(".x", ".y"))
  
  # Use the joined table to look up the CAF
  df_intermediate_lookup<- dplyr::full_join(df_intermediate_df, df_lookup, by = c("facility" = "facility"
                                                                                  , "intervehicle_gap" = "intervehicle_gap"
                                                                                  , "max_platoon_size" = "max_platoon_size"
                                                                                  , "cav_interplatoon_gap" = "cav_interplatoon_gap"
                                                                                  , "cav_percentage" = "cav_percentage"))
  df_intermediate_lookup <- dplyr::filter(df_intermediate_lookup, !is.na(base_capacity.y))
  # Now lets find the values that we want to interpolate for
  df_intermediate_df$caf <- rep(NA, nrow(df_intermediate_df))
  for(r in 1:nrow(df_intermediate_df)){
    row <- df_intermediate_df[r, ]
    message(sprintf("Processing Row %s out of %s", r, nrow(df_intermediate_df)))
    for(c in 1:ncol(row)){
      message(sprintf("|- %s: %s", colnames(row)[c], row[1, c]))
    }
    subset.df <- dplyr::filter(df_intermediate_lookup, facility == row[, L$RoadwaySystem$column_names[1]] & intervehicle_gap == row[, L$CAFLookup$column_names[3]] & max_platoon_size == row[, L$CAFLookup$column_names[4]] & cav_interplatoon_gap == row[, L$CAFLookup$column_names[5]] & cav_percentage == row[, L$CAFLookup$column_names[1]])
    if(nrow(subset.df) == 0){
      message("Could not find a match in the CAF Lookup Table")
      caf <- NA
    } else if(nrow(subset.df) == 1){
      caf <- subset.df[, L$CAFLookup$column_names[7]]
      message(sprintf("Single match on CAF: %s", caf))
    } else {
      input_capacities <- subset.df[, paste0(L$RoadwaySystem$column_names[2],".y")]
      source_capacities <- row[, L$CAFLookup$column_names[6]]
      message(sprintf("Input Capacities: %s", input_capacities))
      message(sprintf("Source Capacities: %s", source_capacities))
      subset.df$difference <- input_capacities - source_capacities
      # Difference values - is there an exact match?
      if(0 %in% subset.df$difference){
        caf <- subset.df$caf[subset.df$difference == 0]
        print(sprintf("Perfect Match on Capacities, CAF: %s", caf))
      } else {
        message("Processing multiple options")
        postive_differences <- subset.df$difference[subset.df$difference > 0]
        negative_differences <- subset.df$difference[subset.df$difference < 0]
        if(length(negative_differences) > 0) {
          min_negative_difference <- max(subset.df$difference[subset.df$difference < 0], na.rm=TRUE)
          min_index <- which(subset.df$difference == min_negative_difference)
          min_weight <- subset.df[min_index, paste0(L$CAFLookup$column_names[6], ".y")]
          min_value <- subset.df[min_index, L$CAFLookup$column_names[7]]
        } else {
          min_weight <- NA
          min_value <- NA
        }
        
        if (length(postive_differences) > 0){
          min_postive_difference <- min(subset.df$difference[subset.df$difference > 0], na.rm=TRUE)
          max_index <- which(subset.df$difference == min_postive_difference)
          max_weight <- subset.df[max_index, paste0(L$CAFLookup$column_names[6], ".y")]
          max_value <- subset.df[max_index, L$CAFLookup$column_names[7]]
        } else {
          max_weight <- NA
          max_value <- NA
        }
        
        
        target_Weight <- row[, L$CAFLookup$column_names[6]]
        message("CHECK INTERPOLATION INPUTS")
        message(sprintf("|- Min Value: %s", min_value))
        message(sprintf("|- Max Value: %s", max_value))
        message(sprintf("|- Min Weight: %s", min_weight))
        message(sprintf("|- Max Weight: %s", max_weight))
        message(sprintf("|- Target Weight: %s", target_Weight))
        caf <- CalculateInterpolatedValue(min_value, max_value, min_weight, max_weight, target_Weight)
        
      }
    }
    if(is.na(caf)){
      df_intermediate_df$caf[r] <- NA
    } else {
      df_intermediate_df$caf[r] <- caf
    }
    
    
  }
  
  df_outlook <- df_intermediate_df %>% select(L$output$column_names[c(1,2,3,4,6)])
  
  # Keep Columns Names
  
  
  # Apply the CAF to the Baseline Capacity
  df_outlook$adjusted_capacity <- round(df_outlook[, L$output$column_names[2]] * df_outlook[, L$output$column_names[4]])
  df_outlook$vehicle_adj_capacity <- round(df_outlook$adjusted_capacity/df_outlook$pce)
  
  df_outlook <- df_outlook[, L$output$column_names]
  
  # Clean up the column names
  colnames(df_outlook) <- L$output$column_description
  if(write_output){
    message(sprintf("WRITING OUTPUT FILE: %s", output))
    write.csv(df_outlook, output)
    
  }
  
  write.csv(df_outlook, output)
  return(df_outlook)
  
}



#' Roadway Capacity, ZOV, and Vehicle Size Adjustments
#' 
#' Modifies the roadway capacity of a specific facility based on the percentage of AVs in the fleet, the connectivity of the infrastrucutre, the driver dynamics associated
#' with the AV fleet, and vehicle size. This function uses two different lookup tables to determine the capacity adjustment factor that should be applied to the baseline roadway capacity
#' @param L A list containing the relatve paths to the input files
#' @param write_output A boolean that indicates if the output file should be written, default is TRUE
#' @return A data frame that provides a summary of the Roadway Capacity Adjustements by Facility Type
#' @import dplyr
CalculateRoadwayCapacityZOVPCE <- function(L, write_output=TRUE){
  
  # Read in the Differnt CSV Files Stored in the List
  df_market_penetration <- read.csv(L$AVMarketPenetration$file, stringsAsFactors = FALSE)
  df_roadway_system <- read.csv(L$RoadwaySystem$file, stringsAsFactors = FALSE)
  df_vehicle_dynamics_lookup <- read.csv(L$VehicleDynamics$file, stringsAsFactors = FALSE)
  df_zov_lookup <- read.csv(L$ZOVCAFLookup$file, stringsAsFactors = FALSE)
  df_lookup <- read.csv(L$CAFLookup$file, stringsAsFactors = FALSE)
  output <- L$output$file
  
  colnames(df_market_penetration) <- L$AVMarketPenetration$column_names
  colnames(df_roadway_system) <- L$RoadwaySystem$column_names
  colnames(df_vehicle_dynamics_lookup) <- L$VehicleDynamics$column_names
  colnames(df_lookup) <- L$CAFLookup$column_names
  colnames(df_zov_lookup) <- L$ZOVCAFLookup$column_names
  
  # Determine the Values for the ZOV Capacity Adjustments
  # Set up the intermediate data.frame
  df_intermediate_df <- df_roadway_system
  
  # Determine the percentage of fleet that is autonomus (level 4 + level 5)
  df_intermediate_df$AV_percentage <- df_market_penetration[5,2] + df_market_penetration[6,2]
  
  # Multiply the minimum value of the global %AV and the AV Allowed for each facility type by the %Connectivity for each facility type.
  cav_percentage <- ((pmin(df_intermediate_df$AV_percentage/100, df_intermediate_df[, L$RoadwaySystem$column_names[4]]/100) * (df_intermediate_df[, L$RoadwaySystem$column_names[3]]/100)))*100
  cav_percentage <- round(cav_percentage, -1)
  df_intermediate_df <- transform(df_intermediate_df, cav_percentage = cav_percentage)
  
  # Add the % AV and % CAV to the Vehicle Dynamics Table
  
  df_intermediate_df <- dplyr::left_join(df_intermediate_df, df_vehicle_dynamics_lookup, by = c("facility" = "facility", "percent_zov" = "percent_zov"), copy = FALSE, suffix = c(".x", ".y"))
  
  # Use the joined table to look up the CAF
  df_intermediate_lookup<- dplyr::full_join(df_intermediate_df, df_zov_lookup, by = c("facility" = "facility"
                                                                           , "intervehicle_gap" = "intervehicle_gap"
                                                                           , "max_platoon_size" = "max_platoon_size"
                                                                           , "cav_interplatoon_gap" = "cav_interplatoon_gap"
                                                                           , "cav_percentage" = "cav_percentage"
                                                                           , "percent_zov" = "percent_zov"))
  df_intermediate_lookup <- dplyr::filter(df_intermediate_lookup, !is.na(base_capacity.y))
  
  # Add an addtional column to the data frame for store the CAFs
  df_intermediate_df$caf <- rep(NA, nrow(df_intermediate_df))
  
  # Iterate through each row and use the lookup table
  for(r in 1:nrow(df_intermediate_df)){
    row <- df_intermediate_df[r, ]
    message(sprintf("Processing Row %s out of %s", r, nrow(df_intermediate_df)))
    for(c in 1:ncol(row)){
      message(sprintf("|- %s: %s", colnames(row)[c], row[1, c]))
    }
    # Subset the Lookup Table to those rows that match the details from the roadway input
    subset.df <- dplyr::filter(df_intermediate_lookup, facility == row[, L$RoadwaySystem$column_names[1]] & intervehicle_gap == row[, L$ZOVCAFLookup$column_names[4]] & max_platoon_size == row[, L$ZOVCAFLookup$column_names[5]] & cav_interplatoon_gap == row[, L$ZOVCAFLookup$column_names[6]] & cav_percentage == row[, L$ZOVCAFLookup$column_names[1]] & percent_zov == row[, L$ZOVCAFLookup$column_names[2]])
    # Check to see how many rows are in the subset
    if(nrow(subset.df) == 0){
      message("Could not find a match in the CAF Lookup Table")
      caf <- NA
    } else if(nrow(subset.df) == 1){
      caf <- subset.df[, L$ZOVCAFLookup$column_names[8]]
      message(sprintf("Single match on CAF: %s", caf))
    } else {
      input_capacities <- subset.df[, paste0(L$RoadwaySystem$column_names[2],".y")]
      source_capacities <- row[, L$ZOVCAFLookup$column_names[7]]
      message(sprintf("Input Capacities: %s", input_capacities))
      message(sprintf("Source Capacities: %s", source_capacities))
      subset.df$difference <- input_capacities - source_capacities
      # Difference values - is there an exact match?
      if(0 %in% subset.df$difference){
        caf <- subset.df$caf[subset.df$difference == 0]
        print(sprintf("Perfect Match on Capacities, CAF: %s", caf))
      } else {
        message("Processing multiple options")
        postive_differences <- subset.df$difference[subset.df$difference > 0]
        negative_differences <- subset.df$difference[subset.df$difference < 0]
        if(length(negative_differences) > 0) {
          min_negative_difference <- max(subset.df$difference[subset.df$difference < 0], na.rm=TRUE)
          min_index <- which(subset.df$difference == min_negative_difference)
          min_weight <- subset.df[min_index, paste0(L$ZOVCAFLookup$column_names[7], ".y")]
          min_value <- subset.df[min_index, L$ZOVCAFLookup$column_names[8]]
        } else {
          min_weight <- NA
          min_value <- NA
        }
        
        if (length(postive_differences) > 0){
          min_postive_difference <- min(subset.df$difference[subset.df$difference > 0], na.rm=TRUE)
          max_index <- which(subset.df$difference == min_postive_difference)
          max_weight <- subset.df[max_index, paste0(L$ZOVCAFLookup$column_names[7], ".y")]
          max_value <- subset.df[max_index, L$ZOVCAFLookup$column_names[8]]
        } else {
          max_weight <- NA
          max_value <- NA
        }
        target_Weight <- row[, L$ZOVCAFLookup$column_names[7]]
        message("CHECK INTERPOLATION INPUTS")
        message(sprintf("|- Min Value: %s", min_value))
        message(sprintf("|- Max Value: %s", max_value))
        message(sprintf("|- Min Weight: %s", min_weight))
        message(sprintf("|- Max Weight: %s", max_weight))
        message(sprintf("|- Target Weight: %s", target_Weight))
        # Interpolate the CAF values
        caf <- CalculateInterpolatedValue(min_value, max_value, min_weight, max_weight, target_Weight)
        
      }
    }
    if(is.na(caf)){
      df_intermediate_df$caf[r] <- NA
    } else {
      df_intermediate_df$caf[r] <- caf
    }
    
    
  }
  
  zov_df <- df_intermediate_df
  
  
  # Remove the intermediate data frames and values
  rm(df_intermediate_df, df_intermediate_lookup, cav_percentage)
  
  # Determine the Values for the Non ZOV Capacity Adjustments
  # Set up the intermediate data.frame
  df_intermediate_df <- df_roadway_system
  
  # Determine the percentage of fleet that is autonomus (level 4 + level 5)
  df_intermediate_df$AV_percentage <- df_market_penetration[5,2] + df_market_penetration[6,2]
  
  # Multiply the minimum value of the global %AV and the AV Allowed for each facility type by the %Connectivity for each facility type.
  cav_percentage <- ((pmin(df_intermediate_df$AV_percentage/100, df_intermediate_df[, L$RoadwaySystem$column_names[4]]/100) * (df_intermediate_df[, L$RoadwaySystem$column_names[3]]/100)))*100
  cav_percentage <- round(cav_percentage, -1)
  df_intermediate_df <- transform(df_intermediate_df, cav_percentage = cav_percentage)
  
  # Add the % AV and % CAV to the Vehicle Dynamics Table
  df_intermediate_df <- dplyr::left_join(df_intermediate_df, df_vehicle_dynamics_lookup, by = c("facility" = "facility", "percent_zov" = "percent_zov"), copy = FALSE, suffix = c(".x", ".y"))
  
  # Use the joined table to look up the CAF
  df_intermediate_lookup<- dplyr::full_join(df_intermediate_df, df_lookup, by = c("facility" = "facility"
                                                                                  , "intervehicle_gap" = "intervehicle_gap"
                                                                                  , "max_platoon_size" = "max_platoon_size"
                                                                                  , "cav_interplatoon_gap" = "cav_interplatoon_gap"
                                                                                  , "cav_percentage" = "cav_percentage"))
  df_intermediate_lookup <- dplyr::filter(df_intermediate_lookup, !is.na(base_capacity.y))
  # Now lets find the values that we want to interpolate for
  df_intermediate_df$caf <- rep(NA, nrow(df_intermediate_df))
  for(r in 1:nrow(df_intermediate_df)){
    row <- df_intermediate_df[r, ]
    message(sprintf("Processing Row %s out of %s", r, nrow(df_intermediate_df)))
    for(c in 1:ncol(row)){
      message(sprintf("|- %s: %s", colnames(row)[c], row[1, c]))
    }
    subset.df <- dplyr::filter(df_intermediate_lookup, facility == row[, L$RoadwaySystem$column_names[1]] & intervehicle_gap == row[, L$CAFLookup$column_names[3]] & max_platoon_size == row[, L$CAFLookup$column_names[4]] & cav_interplatoon_gap == row[, L$CAFLookup$column_names[5]] & cav_percentage == row[, L$CAFLookup$column_names[1]])
    if(nrow(subset.df) == 0){
      message("Could not find a match in the CAF Lookup Table")
      caf <- NA
    } else if(nrow(subset.df) == 1){
      caf <- subset.df[, L$CAFLookup$column_names[7]]
      message(sprintf("Single match on CAF: %s", caf))
    } else {
      input_capacities <- subset.df[, paste0(L$RoadwaySystem$column_names[2],".y")]
      source_capacities <- row[, L$CAFLookup$column_names[6]]
      message(sprintf("Input Capacities: %s", input_capacities))
      message(sprintf("Source Capacities: %s", source_capacities))
      subset.df$difference <- input_capacities - source_capacities
      # Difference values - is there an exact match?
      if(0 %in% subset.df$difference){
        caf <- subset.df$caf[subset.df$difference == 0]
        print(sprintf("Perfect Match on Capacities, CAF: %s", caf))
      } else {
        message("Processing multiple options")
        postive_differences <- subset.df$difference[subset.df$difference > 0]
        negative_differences <- subset.df$difference[subset.df$difference < 0]
        if(length(negative_differences) > 0) {
          min_negative_difference <- max(subset.df$difference[subset.df$difference < 0], na.rm=TRUE)
          min_index <- which(subset.df$difference == min_negative_difference)
          min_weight <- subset.df[min_index, paste0(L$CAFLookup$column_names[6], ".y")]
          min_value <- subset.df[min_index, L$CAFLookup$column_names[7]]
        } else {
          min_weight <- NA
          min_value <- NA
        }
        
        if (length(postive_differences) > 0){
          min_postive_difference <- min(subset.df$difference[subset.df$difference > 0], na.rm=TRUE)
          max_index <- which(subset.df$difference == min_postive_difference)
          max_weight <- subset.df[max_index, paste0(L$CAFLookup$column_names[6], ".y")]
          max_value <- subset.df[max_index, L$CAFLookup$column_names[7]]
        } else {
          max_weight <- NA
          max_value <- NA
        }
        
        
        target_Weight <- row[, L$CAFLookup$column_names[6]]
        message("CHECK INTERPOLATION INPUTS")
        message(sprintf("|- Min Value: %s", min_value))
        message(sprintf("|- Max Value: %s", max_value))
        message(sprintf("|- Min Weight: %s", min_weight))
        message(sprintf("|- Max Weight: %s", max_weight))
        message(sprintf("|- Target Weight: %s", target_Weight))
        caf <- CalculateInterpolatedValue(min_value, max_value, min_weight, max_weight, target_Weight)
        
      }
    }
    if(is.na(caf)){
      df_intermediate_df$caf[r] <- NA
    } else {
      df_intermediate_df$caf[r] <- caf
    }
    
    
  }
  
  nonzov_df <- df_intermediate_df
  
  # Apply Capacity Adjustments Factors
  # ZOV Factors
  zov_df$zov_adjusted_capacity <- round(zov_df$base_capacity*zov_df$caf)
  zov_df$zov_pce_adj_capacity <- round(zov_df$zov_adjusted_capacity/zov_df$pce)
  # Non ZOV Factors
  nonzov_df$adjusted_capacity <- round(nonzov_df$base_capacity*nonzov_df$caf)
  nonzov_df$pce_adj_capacity <- round(nonzov_df$adjusted_capacity/nonzov_df$pce)
  
  # Select only the columns that are needed for the output
  output_df <- nonzov_df[, c("facility", "base_capacity", "cav_percentage", "caf", "adjusted_capacity", "pce", "pce_adj_capacity")]
  output_zov_df <- zov_df[, c("facility", "base_capacity", "cav_percentage", "percent_zov", "caf", "zov_adjusted_capacity" ,"zov_pce_adj_capacity")]
  # Change the name of the ZOV CAF to zov_caf
  cidx <- which(colnames(output_zov_df) == "caf")
  colnames(output_zov_df)[cidx] <- "zov_caf"
  
  
  # Combine the ZOV and Non ZOV data frames
  df_output <- dplyr::left_join(output_df, output_zov_df, by=c("facility" = "facility", "base_capacity" = "base_capacity", "cav_percentage" = "cav_percentage"))
  
  # Set the order of the output
  df_output <- df_output[, L$output$column_names]
  df_output <- as.data.frame(df_output)
  # Change the names to the human-readable 
  colnames(df_output) <- L$output$column_description
  
  # Write the output file
  if(write_output){
    write.csv(df_output, L$output$file)
  }
  # Return the result
  return(df_output)
}





