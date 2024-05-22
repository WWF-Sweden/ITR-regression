# This R-code is built on the the CDP-WWF temperature rating methodoloy, and the code TROPICS-scenario_prep.R available at https://github.com/CDPworldwide/TROPICS-regression/blob/master/TROPICS-scenario_prep.R. 
# This R-code is updated with climate scenario data from AR6 by WWF and CDP for tCDP-WWF temperature rating methodoloy, update 1.5
# This is the first part of the R-code

######################################################################################################################.

#                                           Preparation of data 

######################################################################################################################.

# Description: This script reads, transforms, and generates metadata for AR6 scenario data. The code includes different sections: 
# Section 1: defines path to AR6 data
# Section 2: loads all libraries
# Section 3: contains functions that load and interpolate data, as well as combining variables and generating meta-data
# Section 4.1-4.3 prepare variable lists to import from AR6 data
# Section 4.4 pulls data from the AR6 scenario database
# Section 4.5 calculates the slopes for all chosen variables between 2020 and future years in 5-year increments
# Section 4.6 preparing the data for the second part of the R-code: Regression and plots of data. Data is exported as csv and a summary in xlsx, which in loaded into the script Regression and plots of data. 

# The input data in this R code is:
# AR6_Scenarios_Database_World_v1.1, downloaded from https://data.ece.iiasa.ac.at/ar6/#/login
# AR6_Scenarios_Database_metadata_indicators_v1.1, downloaded from https://data.ece.iiasa.ac.at/ar6/#/login

# The reference of the data: 
# Edward Byers, Volker Krey, Elmar Kriegler, Keywan Riahi, et al.
# AR6 Scenarios Database hosted by IIASA
# International Institute for Applied Systems Analysis, 2022.
# doi: 10.5281/zenodo.5886911 | url: data.ece.iiasa.ac.at/ar6/ DOI

# IEA ETP 2017 data is optionally used to test physical intensity benchmarks in combination with AR6. These data can be acquired from the IEA, but are not necessary to run either of the R scripts included in the repository. Note that this data is currently not being used, but the code is prepared for using such data. 

# Note that the column heading names in the meta data file has been adjusted to not include characters such as brackets () or periods (.), since these characters otherwise can indicate somethine else in R.
# Instead, these signs are replaced by underscore (_) in the variable names. E.g. "Median year of peak warming (MAGICCv7.5.3)" has been replaced with "Median_year_of_peak_warming_MAGICCv7_5_3"  
# This has already been done for the downloaded file 'AR6_Scenarios_Database_metadata_indicators_v1.0.xlsx' on Github, and is not applicable to the world file. 


######################################################################################################################.

# Section 1. Preamble =================================================================================

# clear working environment in R: 
rm(list=ls())

# Set the path for where files should be collected from. The path needs to be updated depending on where the files are stored: 
setwd('name of folder for the library, where the input files are stored')

# Below load data from excel files - AR6 scenario data world and AR6 Metadata - and store them as file paths.
# Load the AR6_Scenarios_Database_World_v1.1 and AR6_Scenarios_Database_metadata_indicators_v1.1 from the excel-files, and giving them the names f_AR6 and f_AR6_meta. Make sure that the names of the files are correct. 
f_AR6 <- 'AR6_Scenarios_Database_World_v1.0.xlsx'
f_AR6_meta <- 'AR6_Scenarios_Database_metadata_indicators_v1.0.xlsx'

# By default, ETP data retrieved from IEA is not used; but if use_etp is set to TRUE, ETP data is combined with AR6 data to calculate a few intensity indicators.
f_etp <- 'ETP2017_industry_summary.xlsx'
use_etp <- FALSE

# Indicates that the code should print processed data to CSV files by default,  
write_csvs <- TRUE

# Section 2. Install packages and load Library =================================================================================

# Install packages:
# install.packages("plyr")
# install.packages("dplyr")
# install.packages("readr")
# install.packages("zeallot")
# install.packages("futile.logger")
# install.packages("magrittr")
# install.packages("ggplot2")
# install.packages("openxlsx")
# install.packages("reshape2")
# install.packages("tidyr")

# Load libraries:
library(plyr)
library(dplyr)
library(readr)
library(zeallot)
library(futile.logger)
library(magrittr)
library(ggplot2)
library(openxlsx)
library(reshape2)
library(tidyr)

# Section 3. Functions ===============================================================================

# 3.1 Data loading functions =================================================================

#### Meta-data #####

# Get AR6 metadata from file 'AR6_Scenarios_Database_metadata_indicators_v1.1', to merge with output data frame, df
# Define a function as 'get.AR6.meta'
# Read data from the sheet meta_Ch3vetted_withclimate, as this sheet contains only scenarios that undergone through IPCC's vetting process. For more information regarding the vetting process, please follow this link: https://www.ipcc.ch/report/ar6/wg3/downloads/report/IPCC_AR6_WGIII_Annex-III.pdf

get.AR6.meta <- function() {
  
  # Mutate code: A new column, Model-Scenario, is created based on the columns Model and Scenario.
  # Select code: Is used to select columns to include i df. -c(1:2) means that the two first columns should be excluded as these two columns are Model and Scenario in the underlying data, and thus replaced by a new columns called Model-Scenario. 
  meta <- (read.xlsx(f_AR6_meta, sheet='meta_Ch3vetted_withclimate') %>%
             mutate(`Model-Scenario` = paste(Model, Scenario, sep='-')) %>%
             select(-c(1:2))
  )
  return(meta)
}

# Create a new variable, 'get.all.data', based on a function of refresh and all_regions:
# Returns a df of AR6 data
# Args:
# refresh (bool variable): if FALSE, get.all.data will use the existing AR6_all_data variable in environment if available. Otherwise, pulls fresh from xlsx.
# all_regions (bool variable): if TRUE, pulls from regions xlsx. Otherwise, pulls world data

get.all.data <- function(refresh, all_regions=FALSE) {
  
  # if(!exists("AR6_all_data") controls if there is a variable with the name AR6_all_data. If this variable does not exist, the condition will be TRUE. 
  # | = IF
  # Refresh is a boolean variable, which indicates if data should be updated and loaded again. If refresh = TRUE, then data will be updated. 
  # In summary, if either the "AR6_all_data" variable does not exist or if refresh is TRUE, the code within the subsequent "if" statement will be executed.
  # If both of these conditions are false (FALSE), the code inside the "if" statement will be skipped and not executed. This is used to determine whether the data should be retrieved again or if existing data can be used.
  if(!all_regions) {
    if(!exists("AR6_all_data") | refresh) {
      
      # Loggmessage:
      
      flog.debug('Pulling AR6 world data from Excel')
      
      
      #### World-data - from file 'AR6_Scenarios_Database_World_v1.1'.#####
      # Read data from f_AR6, previously loaded: 
      AR6_all_data <- read.xlsx(f_AR6)
      
      # Creating a new vector, AR6_ms, and combining the columns Model and Scenario in df AR6_all_data. The name of the column is Model-Scenario. 
      # The code creates the new column in the df AR6_all_data, with the name Model-Scenario.
      
      AR6_ms <- paste(AR6_all_data$Model, AR6_all_data$Scenario, sep='-')
      AR6_all_data <- cbind(AR6_all_data, AR6_ms)
      colnames(AR6_all_data)[ncol(AR6_all_data)] <- "Model-Scenario"
      
      # Note: If the IF-function above is false, the code below will run. 
      # This code executes a similar outcome as above, but from another excel-file, 'AR6_all_regions_all_data'. Such file is not loaded in this code but can be loaded in section 1. 
      
    } else {flog.debug('Using existing AR6_all_data var in environment')}
    return(AR6_all_data) 
  } else {
    if(!exists("AR6_all_regions_all_data") | refresh) {
      flog.debug('Pulling AR6 all regions data from Excel')
      
      AR6_all_regions_all_data <- read.xlsx(f_AR6_all_regions, 1)
      # Make model-scenario column to match with scenarios in SBTi scenario file
      AR6_ms <- paste(AR6_all_regions_all_data$Model, AR6_all_regions_all_data$Scenario, sep='-')
      AR6_all_regions_all_data <- cbind(AR6_all_regions_all_data, AR6_ms)
      colnames(AR6_all_regions_all_data)[ncol(AR6_all_regions_all_data)] <- "Model-Scenario"
    } else{flog.debug('Using existing AR6_all_regions_all_data var in environment')}
    return(AR6_all_regions_all_data) 
  }
}

#### Load ETP data - Note that the ETP data is not currently used #####
get.ETP.data <- function() {
  etp_data0 <- (read.xlsx(f_etp, sheet='WORLD', skipEmptyRows = F, skipEmptyCols = F)[103:109, 15:24])
  
  colnames(etp_data0) <- c('Variable', '2014', seq(2025, 2060, 5))
  return(etp_data0)
  
}

# 3.2 Gap-filler functions ===================================================================

# The function interp.all approximates linear interpolations of missing data for the years 2000-2100. 
# It returns a df with one column per year. Data is interpolated linearly based on spacing of available AR6 data per model-scenario. 

# # The function looks into two main parameters: df (to be interpolated) and id.cols (number of ID columns).
# Args:
# * df (data frame): non-interpolated data -- generally from filter.IPCC
# * id.cols (num vector): numb vector of leading columns to be kept as "ID" columns in returned dataframe
# cdata_yrs_out (bool): if TRUE, returns a list where the second item is a data frame of "keystone years," i.e., for each row, which years are reported data and which are interpolated. 
# If FALSE, function just returns df of interpolated data

interp.all <- function(df, id.cols=5, cdata_yrs_out=FALSE) {
  
  # Two matrices are created: int_df and cd_out, based on Interp.all above. 
  # These are used to store interpolated values and mark which years have actual data.
  # The matrices  has an initial value of 0, and have as many rows as the data set, df, and as many columns as the number of years between 2000-2100. 
  
  int_df <- matrix(0, nrow(df), length(c(2000:2100)))
  cd_out <- matrix(0, nrow(df), length(c(2000:2100)))
  
  # Running a loop through each row of the df to interpolate data. 
  for(i in 1:nrow(df)) { # i is the row index we interpolate
    data_yrs <- colnames(df[i,])[!is.na(df[i,])]
    data_yrs <- unlist(lapply(data_yrs, function(x) {
      x0 <- type.convert(x)
      if(is.numeric(x0)) {return(x0)} else {return(NULL)}
    }))
    cdata_yrs <- as.character(data_yrs)
    
    for(k in 2000:2100) {
      yr_col <- as.character(k)
      if(yr_col %in% cdata_yrs) {
        int_df[i, k-1999] <- df[i, yr_col]
        cd_out[i, k-1999] <- 1
      } else {
        back_yr <- data_yrs[data_yrs < k][
          length(data_yrs[data_yrs < k])]
        forward_yr <- data_yrs[data_yrs > k][1]
        n_yrs <- forward_yr - back_yr
        int_yr <- k - back_yr
        int_data <- (
          df[i, as.character(back_yr)] +
            (int_yr/n_yrs) * (df[i, as.character(forward_yr)] - df[i, as.character(back_yr)]))
        
        if(length(int_data) != 0 & length(int_data) != 1) {
          print(df[i])
          print(int_data)
        }
        
        if(length(int_data) == 0) {
          int_df[i, k-1999] <- NA
        } else{
          int_df[i, k-1999] <- int_data}                                           
      }
      
    }
  }
  
  if(id.cols >1) {
    int_df <- bind_cols(df[,c(1:id.cols)], as.data.frame(int_df)) 
  } else {
    int_df <- cbind(df[,1], as.data.frame(int_df))
    colnames(int_df)[1] <- colnames(df)[1]
  }
  colnames(int_df)[c((id.cols+1):ncol(int_df))] <- sapply(c(2000:2100), as.character)
  if(cdata_yrs_out) {
    return(list(int_df, cd_out))
  } else {
    return(int_df) 
  }
}

# In summary, for each year between 2000 to 2100, the following is executed: 
# a. If the year in column has data (years with actual values), the value from the current column is copied to the corresponding position in int_df, and an indicator is set to 1 in cd_out to indicate that interpolation has NOT been performed for this year.
# b. If the year in column does not have data, a linear interpolation is calculated based on the adjacent years with actual data. The interpolated value is placed in int_df, and an indicator is set to 0 in cd_out to indicate that interpolation has been performed.
# After the loop, the number of  id.cols is checked. If there is more than one identifier column, the function creates a new df by concatenating the identifier columns with the interpolated dataset (int_df). Otherwise, the identifier column is added to the interpolated df. 



#### Calculation of Carbon Storage, CS, from Land use ###### 

# This function adds a column estimating land use-related carbon sequestration, due to poor reporting of 'Carbon Sequestration|Land Use' in AR6 data. This was similar in the SR15 database. 
# This is executed due to a significant amount of negative values, e.g., negative emissions, for the variables Emissions|CO2|AFOLU.   

# The function creates the new column Carbon Sequestration|Land Use2 in the df. The value in the new column is determined by looking at the value in the Emissions|CO2|AFOLU column. If this value is negative, the value in the new column will be its positive counterpart, otherwise it will be 0.

calculate.AFOLU.cs <- function(df) {
  mutate(df, # 'mutate' is used to create new columns or transform existing columns in df. 
         `Carbon Sequestration|Land Use2` = case_when( # 'case_when' executes conditional approaches, similar to 'if else'.   
           `Emissions|CO2|AFOLU` < 0 ~ -`Emissions|CO2|AFOLU`,
           TRUE ~ 0
         )
  )
}


#### Calculation Carbon dioxide removal, CDR ######

# This function calculate total CDR for each row in df, based on the specificed columns and to store the results in a new column, cdr. 
# The function returns a df with a CDR variable, which is a sum of all CDR categories present in the underlying data. 
# Note that this does not include CO2 captured at the point of emissions, e.g., fossil CCS. 

# The function also creates a new variables, CDR_subs, based on a vector with the CDR methods.
# Based on IPCC defintions of CDR method, according to the fact sheet in AR6 -https://www.ipcc.ch/report/ar6/wg3/downloads/outreach/IPCC_AR6_WGIII_Factsheet_CDR.pdf. 

calculate.CDR <- function(df) {
  CDR_subs <- c('CCS|Biomass','Land Use2','Feedstocks',
                'Direct Air Capture', 'Enhanced Weathering', 'Other', "Land Use")
  all_CDR <- generate.varnames('Carbon Sequestration', CDR_subs, FALSE)
  all_CDR <- all_CDR[all_CDR %in% colnames(df)] # Creating names. In the underlying data, the name of the variables starts with Carbon Sequestration, except for the variable Land Use2, instead created above.
  
  df[,all_CDR][is.na(df[,all_CDR])] <- 0   # Missing values or NA values return 0.  
  df$cdr <- apply(df, 1, function(X) {
    sum(as.numeric(X[all_CDR]))
  })
  return(df)
}

#### Calculation of intensity figures ###### 

# This function calculates intensity figures. 

# These intensity figures are based on the underlying data and variables in AR6_Scenarios_Database_World_v1.1 
# (note that all of them are not used in the method but prepared for if you wish to use them)
calculate.intensity.vars <- function(df, use_etp) {
  df_out <- (df %>% mutate(
    INT.emKyoto_gdp=`Emissions|Kyoto Gases`/`GDP|PPP`,
    INT.emCO2EI_PE=`Emissions|CO2|Energy and Industrial Processes`/`Primary Energy`,
    INT.emCO2Elec_elecGen = `Emissions|CO2|Energy|Supply|Electricity`/`Secondary Energy|Electricity`,
    INT.emCO2EI_elecGen = `Emissions|CO2|Energy and Industrial Processes`/`Secondary Energy|Electricity`,
    INT.emCO2Transport_gdp = `Emissions|CO2|Energy|Demand|Transportation`/`GDP|PPP`,
    INT.emCO2EI_cement = `Emissions|CO2|Energy and Industrial Processes`/`Production|Cement`,
    INT.emCO2EI_steel = `Emissions|CO2|Energy and Industrial Processes`/`Production|Steel`,
    INT.emKyoto_m2=`Emissions|Kyoto Gases`/`Energy Service|Residential and Commercial|Floor Space`,
    INT.emKyoto_finalenergy=`Emissions|Kyoto Gases`/`Final Energy`,
    INT.emCO2energysupply_SE=`Emissions|CO2|Energy|Supply`/`Secondary Energy`,
    INT.emKyoto_PE=`Emissions|Kyoto Gases`/`Primary Energy`,
    INT.emCO2energydemand_PE=`Emissions|CO2|Energy|Demand`/`Primary Energy`
    
  ))
 # These intensity figures are based on the ETP data. Note that this file is not used. SS
  if(use_etp) {
    df_out <- (df_out %>% mutate(
      INT.emCO2EI_cement2 = `Emissions|CO2|Energy and Industrial Processes`/`Cement`,
      INT.emCO2IndDemand_cement = `Emissions|CO2|Energy|Demand|Industry`/`Cement`,
      INT.emCO2EI_steel2 = `Emissions|CO2|Energy and Industrial Processes`/`Crude steel`,
      INT.emCO2IndDemand_steel = `Emissions|CO2|Energy|Demand|Industry`/`Crude steel`,
      INT.emCO2EI_aluminum = `Emissions|CO2|Energy and Industrial Processes`/`Total aluminium (primary and secondary)`,
      INT.emCO2IndDemand_aluminum = `Emissions|CO2|Energy|Demand|Industry`/`Total aluminium (primary and secondary)`
    ))
  }
  
  return(df_out)
}

# 3.3 New meta-data ==========================================================================

# This function creates new meta data based on the data previously loaded, and the adjustments and calculations made so far
# The function uses df, slope_vars and slope_year_pairs to create the new variable 'calculate.new.meta'

calculate.new.meta <- function(df, slope_vars, slope_year_pairs) {
  df[, c("cdr|cumulative")] <- NA # A new columns, cdr|cumulative, is added to the df and NA is added as data in that column. 
  for(si in unique(df$`Model-Scenario`)) { # A loop is used for unique values in the column Model-Scenario. Si is used to identify unique combinations of Model-Scenario.     
    
    df[df$`Model-Scenario` == si, "cdr|cumulative"] <- (
      sum(df[df$`Model-Scenario` == si, "cdr"], na.rm = TRUE)) # Calculated the sum of 'cdr|cumulative' for each unique Model-Scenario combination. Missing values are removed. 
    
    df[df$`Model-Scenario` == si, "cdr|max"] <- (
      max(df[df$`Model-Scenario` == si, "cdr"], na.rm = TRUE)) # Calculates max CDR for each unique Model-Scenario combination. na.rm = TRUE or FALSE is used to signal whether missing values should be removed or not. na.rm = TRUE indicates that missing values should be removed
    
    # Next, we test if there are any missing values for 'Emissions|Kyoto Gases' in 2030 for unique Model-Scenario combination (i.e., si)   
    # Handle missing data by setting “Year of max Kyoto emissions” to NA if there are no emissions data for the year 2030 for a given Model-Scenario
    # If there is no missing value for “Emissions|Kyoto Gases” in the year 2030 for a particular Model-Scenario, the function will find the year with maximum emissions and update the “Year of max Kyoto emissions” column with that year. 

    if(is.na(df[df$`Model-Scenario` == si & df$Year == 2030, "Emissions|Kyoto Gases"])) {
      df[df$`Model-Scenario` == si, "Year of max Kyoto emissions"] <- NA
    } else {
      df[df$`Model-Scenario` == si, "Year of max Kyoto emissions"] <- (
        df[df$`Model-Scenario` == si, "Year"][
          df[df$`Model-Scenario` == si, "Emissions|Kyoto Gases"] == max(
            df[df$`Model-Scenario` == si, "Emissions|Kyoto Gases"], na.rm=T
          ) & !is.na(df[df$`Model-Scenario` == si, "Emissions|Kyoto Gases"])])
      
    }
    
    # Execute a similar code as above, but with the variable 'Emissions|CO2|Energy and Industrial Processes', and store the data in the new variable 'Year of max EI CO2 emissions'.
    if(is.na(df[df$`Model-Scenario` == si & df$Year == 2030, "Emissions|CO2|Energy and Industrial Processes"])) {
      df[df$`Model-Scenario` == si, "Year of max EI CO2 emissions"] <- NA
    } else {
      df[df$`Model-Scenario` == si, "Year of max EI CO2 emissions"] <- (
        df[df$`Model-Scenario` == si, "Year"][
          df[df$`Model-Scenario` == si, "Emissions|CO2|Energy and Industrial Processes"] == max(
            df[df$`Model-Scenario` == si, "Emissions|CO2|Energy and Industrial Processes"], na.rm=T
          ) & !is.na(df[df$`Model-Scenario` == si, "Emissions|CO2|Energy and Industrial Processes"])])
    }
  }
  return(df)
}

# In summary, the function above identifies the 'year of max Emissions|Kyoto Gases' and 'Emissions|CO2|Energy and Industrial Processes'. 
# The data is stored in the columns "Year of max Kyoto emissions" and "Year of max EI CO2 emissions" in df. 
# Note that these values are currently not used in the methodology, but is kept for now


# 3.4 Utility functions ======================================================================

# Create a new variable, generate.varnames, based on a function of var0 and subvars. This is used to include variables in df.  
# Returns a vector of IPCC AR6 variables from a nested category
# Args:
# var0 (character): 'Parent' var, e.g. 'Emissions|CO2'
# subvars (chr vector): 'Child' vars, e.g., c('Energy|Supply', 'Energy|Demand')
# include.var0 (bool): whether or not to include var0 w/o any subvars in return

generate.varnames <- function(var0, subvars, include.var0=TRUE) {
  subvars <- sapply(subvars, function(vi)paste(var0, '|', vi, sep=''),
                    USE.NAMES=FALSE)
  
  
  if(include.var0) {
    var_all <- c(var0, subvars)
  } else {
    var_all <- subvars
  }
  
  return(var_all)
}


# 4. Script ==================================================================================

#___4.1 Logging settings =====================================================================
null.result <- flog.threshold(DEBUG, name="ROOT")

#___4.2 Variable lists =======================================================================


#___4.2.1 Emissions variables to include in output dataframe =================================

# Include more variables in df for AR6 compared to SR15
em0 <- 'Emissions|CO2'

# Add additional sector-specific CO2 emissions mitigation variables. Note that all of them is note used in the linear regression output, however, they are kept here if wishes to be used. 
# em0: the 'first string' variable, i.e., Emissions|CO2.
# em_subs: the 'second string' variable, i.e., Emissions|CO2-XXX

em_subs <- c('AFOLU', 'Energy', 'Energy and Industrial Processes', 'Energy|Demand', 'Energy|Demand|Industry', 'Energy|Demand|Industry|Cement','Energy|Demand|Industry|Chemicals',
             'Energy|Demand|Industry|Pulp and Paper', 'Energy|Demand|Industry|Steel', 'Energy|Demand|Residential and Commercial', 'Energy|Demand|Transportation', 'Energy|Demand|Transportation|Aviation',
             'Energy|Demand|Transportation|Maritime', 'Energy|Demand|Transportation|Rail', 'Energy|Demand|Transportation|Road', 'Energy|Supply', 'Energy|Supply|Electricity', 'Industrial Processes', 
             'Industrial Processes|Cement', 'Industrial Processes|Chemicals', 'Industrial Processes|Pulp and Paper', 'Industrial Processes|Steel')

# Visualize data
print(em0)
print(em_subs)


#___4.2.2 Carbon seq variables to include in output data frame ================================
cs0 <- 'Carbon Sequestration'
cs_subs <- c('CCS|Biomass', 'CCS|Biomass|Energy', 'CCS|Biomass|Energy|Supply',
             'CCS|Biomass|Energy|Supply|Electricity', 'CCS|Fossil', 'Land Use',
             'Feedstocks', 'Direct Air Capture', 'Enhanced Weathering','CCS|Industrial Processes', 'Other')


#___4.2.3 Any variables still missing ========================================================
# Some of these variables are used to calculated intensity figures
other_vars <- c('Primary Energy', 'Secondary Energy|Electricity',
                'Emissions|Kyoto Gases', 'Emissions|CH4|AFOLU', 'Emissions|N2O|AFOLU', 'Price|Carbon',
                'Carbon Sequestration|CCS|Biomass|Energy|Supply|Electricity', 'GDP|PPP', 
                'Production|Cement', 'Production|Steel', 'Energy Service|Residential and Commercial|Floor Space', 'Final Energy', 'Secondary Energy')


#___4.3 Prepare arguments for get.scenario.data ==============================================

# All 'first strings' variables (i.e., 'Emissions|CO2')
all0 <- list(
  em0, cs0
)

# All 'second-string' variables (i.e., 'Energy and Industrial Processes')
all_subs <- list(
  em_subs, cs_subs
)

# Generate a new variable 'all_varnames', which combines all0, all_subs and other_vars  
all_varnames <- c((lapply(c(1:length(all0)),
                          function(X) generate.varnames(all0[[X]], all_subs[[X]]))
                   %>% unlist()),
                  other_vars)

# Printing all variable names
print(all_varnames)

# Logg
flog.debug('Looking for %s variables', length(all_varnames))


#____4.4 Pull and transform data  ===================================

# Get all AR6 data and filter on only selected variables into ss0

# Use the already defined function get.all.data, defined in section 3.1, to collect df and filter the data by only including rows where the variables 'all_varnames' are included
# Stored in a new df, ss0
# TRUE implies that data should be updated from its source

ss0 <- get.all.data(TRUE) %>% filter(Variable %in% all_varnames) 


####### Interpolate and transform AR6 data by processing and re-structuring the data in ss0 
flog.debug('Interpolating AR6 data')

# Interpolate, i.e., calculate an approximation for a missing value between two existing values for all variables in the df
# id.cols specifies how many columns that will be used as "Identification columns"
# All processed and re-structured data will be stored in interp_data 

interp_AR6_data <- (ss0 %>% interp.all(id.cols=5) %>% 
                      as.data.frame() %>%   # Structured as a df
                      
                      # Re-structure the data and changing the format (from a wide format to long format through the melt-code)
                      # Year will be the name of the variables
                      melt(id = c(1:5), variable.name='Year') %>%
                      
                      # Re-structure the data and changing the format to a wide format (through dcast). The columns represent unique combinations of Model + Scenario + Year and variable with a value
                      dcast(`Model` + `Scenario` + `Year` ~ `Variable`, value.var='value') %>%    
                      
                      mutate(`Model-Scenario`=paste(`Model`, `Scenario`, sep='-'),   # Combining Model-Scenario and setting Year as a numereric value
                             Year = as.numeric(as.character(Year))
                      ) %>%
                      select(c(ncol(.), 1:(ncol(.)-1))) # Re-structure the data so that the column model-scenario will be the first column
)


# Get ETP data, interpolate, and transform to same structure as AR6 
if(use_etp) {
  flog.debug('Pulling ETP data')
  interp_etp_data <- (get.ETP.data()
                      %>% interp.all(id.cols=1)
                      %>% melt(id.vars='Variable', variable.name = 'Year')
                      %>% dcast(Year ~ Variable)
                      %>% mutate(
                        `Model-Scenario`='ETP-2DS', # Drop "Model-Scenario" from the ETP df because it is being combined with AR6 scenario data to estimate intensity of certain industrial sectors
                        `Model`='ETP',
                        `Scenario`='2DS',
                        Year = as.numeric(as.character(Year))
                      )
                      %>% select(c((ncol(.)-2):ncol(.), 1:(ncol(.)-2)))
                      %>% rename(Cement = 'Cement ')
                      %>% arrange(Year))
  interp_data <- (interp_AR6_data
                  %>% merge(interp_etp_data[, -c(1:3)], by='Year')) # Merge with interpolated AR6 data
  
} else {
  interp_data <- (interp_AR6_data)
}



flog.debug('Calculating new vars')

# Update meta-data based on the processed and re-structured
# Create a new df, AR6_out, based on the processed/re-structured data, interp_data, and add the calculation of calculate.AFOLU.cs(), calculate.CDR() and calculate.intensity.vars() to the df. 
AR6_out <- (interp_data
            %>% calculate.AFOLU.cs()
            %>% calculate.CDR()
            %>% calculate.intensity.vars(use_etp))


# Create an updated version of the meta data for AR6 by adding new columns names from the processes data and combining this with the existing df based on Model-scenario. 
AR6_meta0 <- get.AR6.meta()
AR6_new_meta <- (AR6_out
                 %>% calculate.new.meta())
meta_cols_new <- colnames(AR6_new_meta)[!colnames(AR6_new_meta) %in% colnames(AR6_out)]
AR6_meta <- (AR6_new_meta
             %>% select(c('Model-Scenario', meta_cols_new))
             %>% unique()
             %>% merge(AR6_meta0, by='Model-Scenario'))



#____4.5 Calculate slopes of each variable  ======================


# Create a variable, keep_years, with five years intervals between 2020 - 2050
keep_years <- seq(2020, 2050, 5)

# Create a df, AR6_var_long, based on AR6_out which is the processed/re-structured data and the the calculation of AFOLU.cs(), CDR() & intensity.vars. 
# The df AR6_var_long also filter the data based on the years (2020, 2025, 2030 etc) until 2050.
# melt structure the format to a long format of the data, which facilitates calculations etc. 
# dcast structure the data format back to a wide format.  

# The final output, AR6_var_long, is a df with the years 2020, 2025, 2030, 2035, 2040, 2045 och 2050 as columns, a long with Model, Scenario, Model-Scenario och variable which are "identification columns". 
# This transformation of the data makes it possible to analyse and present the data in an easier way for the specific years.  

AR6_var_long <- (AR6_out %>% filter(Year %in% keep_years)
                 %>% melt(id.vars=c(1:4))
                 %>% mutate(value=as.numeric(value))
                 %>% dcast(Model + Scenario + `Model-Scenario` + variable ~ Year)
)

# Indicate a base year and final year to calculate all slopes
slope_byr <- 2020 # base year
slope_yrf_all <-seq(2025, 2050, 5) # Five year intervals

# Slopes calculated linearly and compound, generate column names
slope_colsLinear <- paste0('slope', sapply(slope_yrf_all, function(X) X-slope_byr))
slope_colsCA <- paste0('slopeCA', sapply(slope_yrf_all, function(X) X-slope_byr))
all_slope_cols <- list(slope_colsLinear, slope_colsCA)

AR6_var_long[, unlist(all_slope_cols)] <- NA

# Pull all data for base year of slope calculation
byr_data <- AR6_var_long[, as.character(slope_byr)]

# Loop through each final year of slope calculation
for(i in c(1:length(slope_colsLinear))) {
  
  # Get column names to be filled
  c(colL, colCA) %<-% c(all_slope_cols[[1]][i], all_slope_cols[[2]][i])
  # Final year of slope calculation in loop
  slope_yrf <- slope_yrf_all[i]
  # Pull data for final year of slope calculation in loop
  yrf_data <- AR6_var_long[, as.character(slope_yrf)]
  
  # Calculate linear reduction and compound reduction
  AR6_var_long[, colL] <- 100 * (yrf_data - byr_data)/byr_data/(slope_yrf-slope_byr)
  AR6_var_long[, colCA] <- 100 * ((yrf_data/byr_data)^(1/(slope_yrf-slope_byr))-1)
  
  # Replace with NA if infinite or over 1000% growth
  AR6_var_long[, colL][is.infinite(AR6_var_long[, colL]) | AR6_var_long[, colL] > 1000] <- NA
  AR6_var_long[, colCA][is.infinite(AR6_var_long[, colL]) | AR6_var_long[, colL] > 1000] <- NA
}

#____4.6 Preparing the data for the second part of the R-code: Regression and plots of data ================================

# Create a new df based on AR6_var_long and merge with AR6 meta data based on Model-Scenario. 
AR6_output <- AR6_var_long %>% merge(AR6_meta, by='Model-Scenario')

# Number of filter combinations
nfilters <- 1  # Update this value if adding more filters.

# Calculate the nscenariosv column
nscenariosv <- length(unique(AR6_output$`Model-Scenario`))

# Print the number of unique scenarios (for control reasons)
print(nscenariosv)

# Add the V8 column to the summary table
AR6_output <- AR6_output %>% mutate(V8 = row_number())

# Write the filtered data to a csv file
if(write_csvs) {
  write_excel_csv(AR6_output, path=paste0('name of folder to store output data/Scenario_dataset_detailed-', nfilters, '.csv'))
}

# Create a summary of the data that includes number of rows and unique scenarios:
summary <- data.frame(V8 = 1:nfilters, nscenariosv = nscenariosv)

# If you want to write the summary to an Excel file
if(write_csvs) {
  write.xlsx(summary, 'name of folder to store output data', overwrite = TRUE)
}


# ======================================= End of script =======================================