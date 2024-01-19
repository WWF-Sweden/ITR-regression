# This code is built on the TROPICS-scenario_prep.R (https://github.com/CDPworldwide/TROPICS-regression/blob/master/TROPICS-scenario_prep.R). 
# This is the first part of the R-code
# Updated for AR6 by WWF - Version 1 

######################################################################################################################.

#                                           Preparation of data 

######################################################################################################################.

# Description: This script reads, transforms, and generates metadata for AR6 scenario data. The code includes different sections: 
# Section 1: defines path to AR6 data and ETP data, which is used for a couple intensity denominators (e.g., steel production)
# Section 2: loads all libraries
# Section 3: contains functions that load and interpolate data, as well as combining variables and generating meta-data
# Section 4.1-4.3 prepare variable lists to import from AR6 data
# Section 4.4 pulls data from the AR6 scenario database and ETP 2017
# Section 4.5 calculates the slopes for all chosen variables between 2020 and future years in 5-year increments
# Section 4.6 produces different subsets of all scenarios based on different combinations of filters, all exported as csv w/ a summary sheet to compare their characteristics. csv's can then be loaded into regression model script

# The input data in this R code is:
  # AR6_Scenarios_Database_World_v1.1, downloaded from https://data.ece.iiasa.ac.at/ar6/#/login
  # AR6_Scenarios_Database_metadata_indicators_v1.1, downloaded from https://data.ece.iiasa.ac.at/ar6/#/login
  # EPT ....NOTE! UPDATE!

# Note that the name of the headings in the underlying data has been adjusted not to include characters such as brackets () or periods (.) as such characters often mean something else in R.
# Instead, underscore (_) has been used in the names.  
 
######################################################################################################################.

# Section 1. Preamble =================================================================================


# clear working environment in R: 
rm(list=ls())

# Below load data from excel files - AR6 scenario data world and AR6 Metadata
# AR6 database fpaths - fpaths is used to sore a list of file paths. 

# Set the path for where files should be collected from. The path needs to be updated depending on where the files are stored: 
setwd("C:/Users/amanda.kihlman/R-test/AR6_Implementering/Input")

# Load the AR6_Scenarios_Database_World_v1.1 and AR6_Scenarios_Database_metadata_indicators_v1.1 from the excel-files, and giving them the names f_AR6 and f_AR6_meta:
f_AR6 <- 'AR6_Scenarios_Database_World_v1.0.xlsx'
f_AR6_meta <- 'AR6_Scenarios_Database_metadata_indicators_v1.0.xlsx'

# If implementing regional-based scenario data, this could be done here: 
# f_AR6_all_regions <- 'add name on file similar as above'

# BELOW NEEDS TO BE UPDATED!
# By default, ETP data is not used; but if a data path is provided and use_etp is set to TRUE,
# ETP data is combined with AR6 data to calculate a few intensity indicators currently under testing.
# use_etp <- FALSE - Indicate that the EPT file is not used, but it provides the opportunity to use it later if needed.  
f_etp <- 'ETP2017_industry_summary.xlsx'
use_etp <- TRUE

# Indicates that the code should print processed data to CSV files by default,  
write_csvs <- TRUE


# Section 2. Install packages and load Library =================================================================================

# Install packages:
install.packages("plyr")
install.packages("dplyr")
install.packages("readr")
install.packages("zeallot")
install.packages("futile.logger")
install.packages("magrittr")
install.packages("ggplot2")
install.packages("openxlsx")
install.packages("reshape2")
install.packages("tidyr")

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

# Meta-data - from file AR6_Scenarios_Database_metadata_indicators_v1.1:

get.AR6.meta <- function() {
  # Get AR6 metadata for merge with output data frame, df
  # Defining a function with the name get.AR6.meta
 
# Read Meta data from file AR6_Scenarios_Database_metadata_indicators_v1.1, from the sheet meta_Ch3vetted_withclimate, as this sheet contains only vetted scenarios. 
  meta <- (read.xlsx(f_AR6_meta, sheet='meta_Ch3vetted_withclimate') %>%
             mutate(`Model-Scenario` = paste(Model, Scenario, sep='-')) %>%
             select(-c(1:2))
  )
  return(meta)
}

  # Mutate code: A new column, Model-Scenario, is created based on the columns Model and Scenario.
  # Select code: Is used to select columns to include i df. -c(1:2) means that the two first columns should be excluded as these two columns are Model and Scenario in the underlying data, and thus replaced by a new columns called Model-Scenario. 

# Creating a variables, get.all.data based on a function of refresh and all_regions:
get.all.data <- function(refresh, all_regions=FALSE) {
  # Returns a df of AR6 data
  # Args:
  # refresh (bool variable): if FALSE, get.all.data will use the existing AR6_all_data variable in environment if available. Otherwise, pulls fresh from xlsx.
  # all_regions (bool variable): if TRUE, pulls from regions xlsx. Otherwise, pulls world data

    if(!all_regions) {
    if(!exists("AR6_all_data") | refresh) {
      
  # if(!exists("AR6_all_data") control if there is a variable with the name AR6_all_data. If this variable does not exist, the condition will be TRUE. 
  # This | means IF
  # Refresh is a boolean variable, which indicates if data should be updated and loaded again. If refresh = TRUE, then data will be updated. 
  # In summary, if either the "AR6_all_data" variable does not exist or if refresh is TRUE, the code within the subsequent "if" statement will be executed.
  # If both of these conditions are false (FALSE), the code inside the "if" statement will be skipped and not executed. This is used to determine whether the data should be retrieved again or if existing data can be used.
      
  
# Loggmessage:
      
      flog.debug('Pulling AR6 world data from Excel')
    
# World-data - from file AR6_Scenarios_Database_World_v1.1:
      
  # Read data from th excel-file f_AR6, which was previously loaded: 
      AR6_all_data <- read.xlsx(f_AR6)
   
      AR6_ms <- paste(AR6_all_data$Model, AR6_all_data$Scenario, sep='-')
      AR6_all_data <- cbind(AR6_all_data, AR6_ms)
      colnames(AR6_all_data)[ncol(AR6_all_data)] <- "Model-Scenario"
      
  # Creating a new vector, AR6_ms, and combining the columns Model and Scenario in df AR6_all_data. The name of the column is Model-Scenario. 
  # The code creates the new column in the df AR6_all_data, with the name Model-Scenario.
      
  # Note: If the IF-function above is false, the code below will run. 
  # The code below execute a similar outcome as above, but from another excel-file, AR6_all_regions_all_data. Such file is not loaded in this code but can be loaded in section 1. 
      
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

# Load ETP data - NOTE! THIS FILE IS FROM 2017 - IF THIS SHOULD BE USED WE SHOULD PROBABLY UPDATE IT WITH A NEW FILE

# Read the file from the sheet "WORLD" and skip empty rows. 
# Index is used to find which rows that should be collected and from which columns. 
# Data is stored in a df called etp_data0.
get.ETP.data <- function() {
  etp_data0 <- (read.xlsx(f_etp, sheet='WORLD', skipEmptyRows = F, skipEmptyCols = F)[103:109, 15:24])
 
  colnames(etp_data0) <- c('Variable', '2014', seq(2025, 2060, 5))
  
  # The code above changes the column names in the etp_data0 df to new names specified in a vector. 
  # The first column will be named Variable, and then follows a sequence of years from 2014-2060 with a step size of 5 years.
  # The data is retuned in a df 
  return(etp_data0)

}

# 3.2 Gap-filler functions ===================================================================

# The code-block below defines a function, interp.all, with the purpose to interpolate data in df over the time interval 2000-2100.
# The function interp.all looks into two main parameters: df (data frame which will be interpolated) and id.cols (the number of ID columns).

interp.all <- function(df, id.cols=5, cdata_yrs_out=FALSE) {
  # Returns a dataframe with one column per year between 2000 and 2100
  # Where data is interpolated linearly based on spacing of available
  # AR6 data per model-scenario. (I.e., works with 5-year or 10-year data or mixed.)
  # Args:
  # * df (dataframe): non-interpolated data -- generally from filter.IPCC
  # * id.cols (num vector): numb vector of leading columns to be kept as "ID" columns in returned dataframe
  # cdata_yrs_out (bool): if TRUE, returns a list where the second item is a dataframe of "keystone years," i.e., for each row, which years are reported data and which are interpolated. 
  # If FALSE, function just returns df of interpolated data
  

# Inside the function, two matrices are created: int_df and cd_out, based on Interp.all above. 
# These are used to store interpolated values and mark which years have actual data.
  int_df <- matrix(0, nrow(df), length(c(2000:2100)))
  cd_out <- matrix(0, nrow(df), length(c(2000:2100)))
  # The matrices  has an initial value of 0, and have as many rows as the data set, df, and as many columns as the number of years between 2000-2100. 
  
  
# Running a loop through each row of the df to interpolate data if needed. 
  for(i in 1:nrow(df)) {
    # Row index to write to
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

  # For each row, column names (years) with actual values/data is extracted and converted to numeric values. These columns represent the years that have actual values in df.
  # For each year in the interval 2000 to 2100, the following is executed: 
    # a. If the year in column has data (years with actual values), the value from the current column is copied to the corresponding position in int_df, and an indicator is set to 1 in cd_out to indicate that interpolation has NOT been performed for this year.
    # b. If the year in column does has data, a linear interpolation is calculated based on the adjacent years with actual data. The interpolated value is placed in int_df, and an indicator is set to 0 in cd_out to indicate that interpolation has been performed.
  # After all loop is completed, the number of  id.cols is checked. If there is more than one identifier column, the function creates a new df by concatenating the identifier columns with the interpolated dataset (int_df). Otherwise, the identifier column is added to the interpolated df. 
  # The name of the columns in the interpolated df changes to match the years from 2000 to 2100. 

# In summary, this code support conducting linear interpolations of the missing data for the years between 2000-2100, taking actual data into consideration as the basis of the calculation.
# This is useful for filling in missing values and creating an time distribution of data.


#### Calculation of Carbon Storage, CS, from Land use ###### 

calculate.AFOLU.cs <- function(df) {
  # Add a column estimating land use-related carbon sequestration due to poor reporting of 'Carbon Sequestration|Land Use' in AR6 data. 
  # This was similar in the SR1.5 database. 
  # This is executed due to a significant amount of negative values, e.g., negative emissions, for the variables Emissions|CO2|AFOLU.   
  
  mutate(df,
         `Carbon Sequestration|Land Use2` = case_when(
           `Emissions|CO2|AFOLU` < 0 ~ -`Emissions|CO2|AFOLU`,
           TRUE ~ 0
         )
  )
}
  # The code Mutate is used to created new columns or transform existing columns in df. 
  # In the above code, a new column is created Carbon Sequestration|Land Use2.
  # The code case_when is used for execute conditional approaches, similar as if else.   
  # Emissions|CO2|AFOLU is the current column in df.
  # If the value in the column Emissions|CO2|AFOLU is lower than 0, e.g., negative, this value will be transferred to the new column Carbon Sequestration|Land Use2 but with a positive sign. 
  # If the value in the column Emissions|CO2|AFOLU is, on the other hand, larger than 0, the transferred value to the new column Carbon Sequestration|Land Use2 will be zero. 
  
# In summary, this code block creates new column Carbon Sequestration|Land Use2 in the df. The value in the new column is determined by looking at the value in the Emissions|CO2|AFOLU column (in the underlying data). If this value is negative, the value in the new column will be its positive counterpart, otherwise it will be 0.


#### Calculation Carbon dioxide removal, CDR ###### 

# The code block below is used to calculate total CDR for each row in df, based on the specificed columns and to store the results in a new column, cdr. 
calculate.CDR <- function(df) {
  # Return a df with a CDR variable, which is a sum of all CDR categories present in the underlying data. 
  # Note that this does not include CO2 captured at the point of emissions, e.g., fossil CCS. 
  
  CDR_subs <- c('CCS|Biomass','Land Use2','Feedstocks',
                'Direct Air Capture', 'Enhanced Weathering', 'Other', "Land Use")
  # Creating a new variables, CDR_subs, based on a vector with the CDR methods.
  # Compared to SR1.5, Carbon Sequestration|Land use is added as the IPCC is defining it as CDR method, according to the fact sheet in AR6 -https://www.ipcc.ch/report/ar6/wg3/downloads/outreach/IPCC_AR6_WGIII_Factsheet_CDR.pdf. 
  
  
  all_CDR <- generate.varnames('Carbon Sequestration', CDR_subs, FALSE)
  all_CDR <- all_CDR[all_CDR %in% colnames(df)]
  # Creating names. In the underlying data, the name of the variables starts with Carbon Sequestration, except for the variable Land Use2 as that variable was created above.   
  
  df[,all_CDR][is.na(df[,all_CDR])] <- 0
  # Missing values or NA values return 0.  
  
  df$cdr <- apply(df, 1, function(X) {
    sum(as.numeric(X[all_CDR]))
   
  })
  return(df)
}

#### Calculation of intensity figures ###### 

# Below calculates intensity figures. 
# Some of the calculations are based on the ETP file, loaded in section 1. If this code is TRUE: use_etp <- TRUE (in section 1), then the code block below will be executed. 
calculate.intensity.vars <- function(df, use_etp) {

# The intensity figures are based on the underlying data and variables in AR6_Scenarios_Database_World_v1.1: 
  df_out <- (df %>% mutate(
    INT.emKyoto_gdp=`Emissions|Kyoto Gases`/`GDP|PPP`,
    INT.emCO2EI_PE=`Emissions|CO2|Energy and Industrial Processes`/`Primary Energy`,
    INT.emCO2Elec_elecGen = `Emissions|CO2|Energy|Supply|Electricity`/`Secondary Energy|Electricity`,
    INT.emCO2EI_elecGen = `Emissions|CO2|Energy and Industrial Processes`/`Secondary Energy|Electricity`,
    INT.emCO2Transport_gdp = `Emissions|CO2|Energy|Demand|Transportation`/`GDP|PPP`
  ))
  
# The intensity figures are based on the ETP data. NOTE! PERHAPS WE NEED TO UPDATE THIS DATA AS IT IS FROM 2017. 
  if(use_etp) {
    df_out <- (df_out %>% mutate(
      INT.emCO2EI_cement = `Emissions|CO2|Energy and Industrial Processes`/`Cement`,
      INT.emCO2IndDemand_cement = `Emissions|CO2|Energy|Demand|Industry`/`Cement`,
      INT.emCO2EI_steel = `Emissions|CO2|Energy and Industrial Processes`/`Crude steel`,
      INT.emCO2IndDemand_steel = `Emissions|CO2|Energy|Demand|Industry`/`Crude steel`,
      INT.emCO2EI_aluminum = `Emissions|CO2|Energy and Industrial Processes`/`Total aluminium (primary and secondary)`,
      INT.emCO2IndDemand_aluminum = `Emissions|CO2|Energy|Demand|Industry`/`Total aluminium (primary and secondary)`
    ))
  }
  
  return(df_out)
}

# 3.3 New meta-data ==========================================================================

# This part of the section creates new meta data based on the data previously loaded to the code, and the adjustments and calculations made so far. 


calculate.new.meta <- function(df, slope_vars, slope_year_pairs) {
  # A new variable is created, calculate.new.meta,  based on a function of df, slope_vars and slope_year_pairs. 
  
  df[, c("cdr|cumulative")] <- NA
  # A new columns, cdr|cumulative, is added to the df and NA is added as data in that column. 
  
  for(si in unique(df$`Model-Scenario`)) {
    # A loop is used for unique values in the column Model-Scenario. Si is used to identify unique combinations of Model-Scenario.     
    
    
    df[df$`Model-Scenario` == si, "cdr|cumulative"] <- (
      sum(df[df$`Model-Scenario` == si, "cdr"], na.rm = TRUE))
      # The above code calculate the sum of cdr|cumulative for each unique Model-Scenario combination. Missing values are removed. 
    
    df[df$`Model-Scenario` == si, "cdr|max"] <- (
      max(df[df$`Model-Scenario` == si, "cdr"], na.rm = TRUE))
      # The above code calculated max cdr|max for each unique Model-Scenario combination. na.rm = TRUE or FALSE is used to signal whether or not missing values should be removed or not. na.rm = TRUE means that missing values should be removed.    
    
    if(is.na(df[df$`Model-Scenario` == si & df$Year == 2030, "Emissions|Kyoto Gases"])) {
      df[df$`Model-Scenario` == si, "Year of max Kyoto emissions"] <- NA
      # The above code tests if there are any missing values for Emissions|Kyoto Gases in 2030 for unique Model-Scenario combination (i.e., si)   
      # The code is used to handle missing data by setting the “Year of max Kyoto emissions” to NA if there are no emissions data for the year 2030 for a given Model-Scenario. 
      # If there are missing value, i.e.,the if-function is true,"Year of max Kyoto emissions" for that specific row will be set to NA.   
      
    } else {
      df[df$`Model-Scenario` == si, "Year of max Kyoto emissions"] <- (
      df[df$`Model-Scenario` == si, "Year"][
      df[df$`Model-Scenario` == si, "Emissions|Kyoto Gases"] == max(
          df[df$`Model-Scenario` == si, "Emissions|Kyoto Gases"], na.rm=T
        ) & !is.na(df[df$`Model-Scenario` == si, "Emissions|Kyoto Gases"])])
      
    }
    # The case where there are emission values present for the year 2030, the above code will be executed. 
    # If there is no missing value for “Emissions|Kyoto Gases” in the year 2030 for a particular Model-Scenario, the code will find the year with the maximum emissions and update the “Year of max Kyoto emissions” column with that year. 
    # If the maximum value cannot be determined due to missing data, the “Year of max Kyoto emissions” is set to NA as handled by the previous if condition. 
    # This code is part of a data analysis process to determine the peak emissions year for different scenarios.
    # In summary, the code is building a column "Year of max Kyoto emissions" in df, where the year of the maximum value of the emissions in Emissions|Kyoto Gases is added (based on the unique combination of Model-Scenario). 
    
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
    # The above execute a similar code as with Emissions|Kyoto Gases, but with the variable Emissions|CO2|Energy and Industrial Processes, and store the data in 'Year of max EI CO2 emissions'.
    # The "Year of max EI CO2 emissions" is created. 
  
  return(df)
}

# In summary, the code is used to identify the year of max Emissions|Kyoto Gases and Emissions|CO2|Energy and Industrial Processes.
# The data is stored in the columns "Year of max Kyoto emissions" and "Year of max EI CO2 emissions" in df. 


# 3.4 Utility functions ======================================================================

# Creating a new variable, generate.varnames, based on a function of var0 and subvars. This is used to include variables in df.  
generate.varnames <- function(var0, subvars, include.var0=TRUE) {
  
  # Returns a vector of IPCC AR6 variables from a nested category
  # Args
  # var0 (character): 'Parent' var, e.g. 'Emissions|CO2'
  # subvars (chr vector): 'Child' vars, e.g., c('Energy|Supply', 'Energy|Demand')
  # include.var0 (bool): whether or not to include var0 w/o any subvars in return
  
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

# Variables included in df for AR6 is the same as for SR1.5
em0 <- 'Emissions|CO2'
em_subs <- c('Energy and Industrial Processes', 'Energy', 'Industrial Processes',
             'Energy|Supply', 'Energy|Demand', 'Energy|Demand|Industry', 'Energy|Demand|Transportation',
             'Energy|Supply|Electricity', 'AFOLU')

  # Based on variables in the underlying data.
  # em0: the 'first string' variable, i.e., Emissions|CO2.
  # em_subs: the 'second string' variable, i.e., Emissions|CO2-XXX

#Below is only for test purposes:
print(em0)
print(em_subs)


#___4.2.2 Carbon seq variables to include in output data frame ================================
cs0 <- 'Carbon Sequestration'
cs_subs <- c('CCS|Biomass', 'CCS|Biomass|Energy', 'CCS|Biomass|Energy|Supply',
             'CCS|Biomass|Energy|Supply|Electricity', 'CCS|Fossil', 'Land Use',
             'Feedstocks', 'Direct Air Capture', 'Enhanced Weathering','CCS|Industrial Processes', 'Other')


#___4.2.3 Any variables still missing ========================================================
other_vars <- c('Primary Energy', 'Secondary Energy|Electricity',
                'Emissions|Kyoto Gases', 'Emissions|CH4|AFOLU', 'Emissions|N2O|AFOLU', 'Price|Carbon',
                'Carbon Sequestration|CCS|Biomass|Energy|Supply|Electricity', 'GDP|PPP')

  # Some of the variables above are used to calculated intensity figures. 

#___4.3 Prepare arguments for get.scenario.data ==============================================

# All 'first strings' variables (i.e., Emissions|CO2). 
all0 <- list(
  em0, cs0
)

# All 'second-string' variables (i.e., Energy and Industrial Processes)
all_subs <- list(
  em_subs, cs_subs
)

# Generating a variables, all_varnames, which combines all0, all_subs and other_vars  
all_varnames <- c((lapply(c(1:length(all0)),
                           function(X) generate.varnames(all0[[X]], all_subs[[X]]))
                    %>% unlist()),
                  other_vars)

# Printing all variable names (for test only)
print(all_varnames)

# Logg
flog.debug('Looking for %s variables', length(all_varnames))


#____4.4 Pull and transform data according to Andres specs ===================================

# Get all AR6 data and filter to only the variables selected.
# Calling the function get.all.data, previously defined in section 3.1. 

ss0 <- get.all.data(TRUE) %>% filter(Variable %in% all_varnames)
  # TRUE implies that data should be updated, refreshed, from its source. 
  # The data is filtered based on the variables selected in all_varnames. 

# In summary,the code collect df through calling on the function get.all.data, updates it if needed, and filter the data by only including rows where the variables in  all_varnames are included.  
# The results is stored in a new df, ss0.


# Interpolate and transform AR6 data. The code below processes and re-strucutre the data in ss0 

flog.debug('Interpolating AR6 data')

interp_AR6_data <- (ss0 %>% interp.all(id.cols=5) %>%
  # Interpolating, i.e., calculating a value between two existing values with actual data, for all variables in the df. 
  # id.cols specifies how many columns that will be used as "Identification columns". 
    as.data.frame() %>%
    # This is then structured as a df.  
    
    melt(id = c(1:5), variable.name='Year') %>%
    # Re-structure the data and changing the format (from a wide format to long format through the melt-code). 
    # Year will be the name of the variables. 
  
    dcast(`Model` + `Scenario` + `Year` ~ `Variable`, value.var='value') %>%
    # Re-structure the data and changing the format to a wide format (through dcast). The columns representes unique combinations of Model + Scenario + Year and variable with a value.  
    
     #Nedan slår ihop model och scenario till Model-Scenario, och year som ett numeriskt värde. Läses från vänster till höger. 
   mutate(`Model-Scenario`=paste(`Model`, `Scenario`, sep='-'),
           Year = as.numeric(as.character(Year))
    ) %>%
  # Combining Model-Scenario and setting Year as a numereric value. 

        select(c(ncol(.), 1:(ncol(.)-1)))
    )
    # Re-structure the data so that the column model-scenario will be the first column

# All processed and re-structured data will be stored in interp_AR6_data.  

# Get ETP data, interpolate, and transform to same structure as AR6
if(use_etp) {
  flog.debug('Pulling ETP data')
  interp_etp_data <- (get.ETP.data()
                      %>% interp.all(id.cols=1)
                      %>% melt(id.vars='Variable', variable.name = 'Year')
                      %>% dcast(Year ~ Variable)
                      %>% mutate(
                        `Model-Scenario`='ETP-2DS',
                        `Model`='ETP',
                        `Scenario`='2DS',
                        Year = as.numeric(as.character(Year))
                      )
                      %>% select(c((ncol(.)-2):ncol(.), 1:(ncol(.)-2)))
                      %>% rename(Cement = 'Cement ')
                      %>% arrange(Year))

  # Merge AR6 data (interp_AR6_data) with ETP data (interp_etp_data).  
  # We are dropping "Model-Scenario" from the ETP dataframe because it is being combined with AR6 scenario data to estimate intensity of certain industrial sectors.
  interp_data <- (interp_AR6_data
                  %>% merge(interp_etp_data[, -c(1:3)], by='Year'))
  
} else {
  interp_data <- (interp_AR6_data)
}

flog.debug('Calculating new vars')

# Updating meta-data based on the processed and re-structured: 
AR6_out <- (interp_data
             %>% calculate.AFOLU.cs()
             %>% calculate.CDR()
             %>% calculate.intensity.vars(use_etp))
  # Creating a new df, AR6_out, based on the processed/re-structured data, interp_data, and add the calculation of calculate.AFOLU.cs(), calculate.CDR() and calculate.intensity.vars(use_etp) to the df. 

AR6_meta0 <- get.AR6.meta()
AR6_new_meta <- (AR6_out
              %>% calculate.new.meta())
meta_cols_new <- colnames(AR6_new_meta)[!colnames(AR6_new_meta) %in% colnames(AR6_out)]
AR6_meta <- (AR6_new_meta
              %>% select(c('Model-Scenario', meta_cols_new))
              %>% unique()
              %>% merge(AR6_meta0, by='Model-Scenario'))

  # The code above creates an updated version of the meta data for AR6 by adding new columns names from the processes data and combining this with the existing df based on Model-scenario. 
  # This provides a complete and updated set of metadata for the processed AR6 data. 

#____4.5 Calculate slopes of each variable and transform to Chris specs ======================

keep_years <- seq(2020, 2050, 5)
  # Creating a variable, keep_years, with five years intervals between 2020 - 2050
AR6_var_long <- (AR6_out %>% filter(Year %in% keep_years)
              %>% melt(id.vars=c(1:4))
              %>% mutate(value=as.numeric(value))
              %>% dcast(Model + Scenario + `Model-Scenario` + variable ~ Year)
)
  # Creating a df, AR6_var_long, based on AR6_out which is the processed/re-structured data and the the calculation of AFOLU.cs(), CDR() & intensity.vars. 
  # The df AR6_var_long also filter the data based on the years (2020, 2025, 2030 etc) until 2050.
  # melt structure the format to a long format of the data, which facilitates calculations etc. 
  # dcast structure the data format back to a wide format.  

  # The final output, AR6_var_long, is a df with the years 2020, 2025, 2030, 2035, 2040, 2045 och 2050 as columns, a long with Model, Scenario, Model-Scenario och variable which are "identification columns". 
  # This transformation of the data makes it possible to analyse and present the data in an easier way for the specific years.  

# Indicate a base year and final year to calculate all slopes
slope_byr <- 2020
  # base year
slope_yrf_all <-seq(2025, 2050, 5)
  # Five year intervals

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

#____4.6 Filter based on different combinations of parameters ================================
# Note that the filters below is almost the same as in the SR1.5 version of the code, however, adjusted to AR6 data. 
# Only one exception: CDR variable (see below) 

# Creating a new df based on AR6_var_long which is merged with AR6 meta data based on Model-Scenario. 
AR6_prefilter <- AR6_var_long %>% merge(AR6_meta, by='Model-Scenario')

# Latest allowed peak year of emissions:
latest_peak <- c(2100, 2020, 2025, 2030)


# Mitigation scenario:
applied_to <- list(c('C1: limit warming to 1.5°C (>50%) with no or limited overshoot' , 'C2: return warming to 1.5°C (>50%) after a high overshoot' , 'C3: limit warming to 2°C (>67%)' , 'C4: limit warming to 2°C (>50%)' , 'C5: limit warming to 2.5°C (>50%)' , 'C6: limit warming to 3°C (>50%)' , 'C7: limit warming to 4°C (>50%)' , 'C8: exceed warming of 4°C (>=50%)'),
                   c('C1: limit warming to 1.5°C (>50%) with no or limited overshoot' , 'C2: return warming to 1.5°C (>50%) after a high overshoot' , 'C3: limit warming to 2°C (>67%)' , 'C4: limit warming to 2°C (>50%)'),
                   c('C1: limit warming to 1.5°C (>50%) with no or limited overshoot' , 'C2: return warming to 1.5°C (>50%) after a high overshoot')
                   )


# Emissions peak variable:
which_peak_var <- c("Year of max Kyoto emissions", "Year of max EI CO2 emissions")

# Allowed level of CDR (negative) (GT CO2/yr)
min_co2 <- c(-1000, -20, -15, -10)

# CDR variable
which_cdr_var <- c("cdr|max")
  # NOTE: Deleted"minimum.net.CO2.emissions (Gt.CO2/yr) as this variable does not exist in AR6 data. 

# Data collected for output df

nvariations <- (length(latest_peak)*length(applied_to)*length(which_peak_var)*
                  length(min_co2)*length(which_cdr_var))
  # This variable count the number of variations to be used when creating the data sets. Total 96 variations, hence 96 data sets. 

filtered_dfs <- vector(mode='list', length = nvariations)
  # A list including df for the different variations. 

c(peak_yrv, peak_varv, appliedv, cdr_limv, cdr_varv, nscenariosv) %<-% (
  lapply(c(1:6), function(X) vector(mode='character', length=nvariations)))

# In summary, a list of 6 elements is created, each a vector of type "character" of length nvariations. 
# Then "list destructuring" is used to assign each vector's values to the corresponding variables (peak_yrv, peak_varv, etc.).
# This makes it possible to easily create multiple variables with the same basic properties and length without having to declare them one by one.

counter <- 0

# To control the source where the files should be stored. 
getwd()

# View the column names in AR6_prefilter -  only a control 
colnames(AR6_prefilter)

# Loop through each filter: 
for(pi in which_peak_var){
  for(ci in which_cdr_var) {
    for(ai in applied_to) {
      for(peak_yr in latest_peak) {
        for(cdr_val in min_co2) {
          
          # The below is updated to fit AR6 data:
          counter <- counter + 1
          if(length(ai) > 4) {
            pscenarios <- 'Above 2C'
          } else if(length(ai) > 3){
            
            pscenarios <- '2C'} else {
              pscenarios <- '1.5C'
          }
          
          set_name <- paste0(pi,' for ', pscenarios, ' scenarios: ', peak_yr,
                            '. CDR less than ', -cdr_val, ' GT CO2/yr based on ', ci)
          
          peak_yrv[counter] <- peak_yr
          peak_varv[counter] <- pi
          appliedv[counter] <- pscenarios
          cdr_limv[counter] <- ci
          cdr_varv[counter] <- cdr_val
          
          if(ci == 'cdr|max') {
            filtered_dfi <- AR6_prefilter %>% filter(
              ((!! rlang::sym(pi) <= peak_yr) & (`cdr|max` <= -cdr_val*1000) & (Category_name %in% ai)) |
                ((`cdr|max` <= -cdr_val*1000) & !Category_name %in% ai))
          } else {
            if("minimum.net.CO2.emissions.(Gt.CO2/yr)" %in% colnames(AR6_prefilter)) {
              filtered_dfi <- AR6_prefilter %>% filter(
                ((!! rlang::sym(pi) <= peak_yr) & (`minimum.net.CO2.emissions.(Gt.CO2/yr)` >= cdr_val) & (Category_name %in% ai)) |
                  ((`minimum.net.CO2.emissions.(Gt.CO2/yr)` >= cdr_val) & (!Category_name %in% ai)))
            } else {
              filtered_dfi <- AR6_prefilter %>% filter(
                (!! rlang::sym(pi) <= peak_yr) & (Category_name %in% ai))
            }
          }
          
          filtered_dfs[[counter]] <- filtered_dfi
          names(filtered_dfs)[counter] <- set_name
          
         # print(length(unique(filtered_dfi$`Model-Scenario`)))
          nscenariosv[counter] <- length(unique(filtered_dfi$`Model-Scenario`))
          
          
          # Creating csv-files for each unique combination of the filters
          if(write_csvs) {
            write_excel_csv(filtered_dfi, path=paste0('TROPICS-scenario_data_csv/TROPICS_dataset-', counter, '.csv')) 
          }
        }
      }   
    }
  }
}

# Print the filters for each data set and create a csv-file: NOTE UPDATE NAME FROM TROPICS! 

mapping <- cbind(names(filtered_dfs), peak_yrv, peak_varv, appliedv, cdr_limv,
                 cdr_varv, nscenariosv, seq(1, length(filtered_dfs)))

if(write_csvs) {
  write.xlsx(as.data.frame(mapping), 'TROPICS-scenario_data_csv/TROPICS_dataset_mapping.xlsx', overwrite = TRUE)
}





