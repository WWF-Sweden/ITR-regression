# This R-code is built on the the CDP-WWF temperature rating methodoloy, and the code ROPICS-regression /Regression_plots_and_models_SR15TempScoring_vAndres.R available at https://github.com/CDPworldwide/TROPICS-regression/blob/master/Regression_plots_and_models_SR15TempScoring_vAndres.R. 
# This R-code is updated with climate scenario data from AR6 by WWF and CDP for CDP-WWF temperature rating methodoloy, update 1.5
# This is the second part of the R-code.

# The reference of the data: 
# Edward Byers, Volker Krey, Elmar Kriegler, Keywan Riahi, et al.
# AR6 Scenarios Database hosted by IIASA
# International Institute for Applied Systems Analysis, 2022.
# doi: 10.5281/zenodo.5886911 | url: data.ece.iiasa.ac.at/ar6/ DOI

######################################################################################################################.

#                                           Regression and plots of data 

######################################################################################################################.

# Description: This script creates regressions and plots of AR6 scenario data, 
# built on the output from AR6 Implementation_Preparation of data. The code includes different sections: 
# Section 1. Preparation: install relevant packages and load libraries. 
# Section 2. Load data output files generated in the first part of the R-code 
# (Preparation of data) and xlsx created based on the underlying input AR data.   
# Section 3. Defining benchmarks and variables for plotting.
# Section 4. Plotting the data
# Section 5. Create and store regression models
# Section 6. Save final output

# Section 1. Preparation  =================================================================================

# clear working environment in R: 
rm(list=ls())

# Set the path for where files should be collected from. The path needs to be 
# updated depending on where the files are stored: 
setwd('C:/Users/julia.behm/Documents/R/Data')

#Install and load packages
install.packages("ggplot2")
install.packages("ggthemes")
install.packages("plyr")
install.packages("reshape2")
install.packages("stringer")
install.packages("zoo")
install.packages("dplyr")
install.packages("tidyr")
install.packages("openxlsx")
install.packages("magrittr")

library(ggplot2)
library(ggthemes)
library(plyr)
library(reshape2)
library(stringr)
library(zoo)
library(dplyr)
library(tidyr)
library(openxlsx)
library(magrittr)

# Section 2. Load data files  =================================================================================

# filepath to data set from the first part of the R-code:
f_datamap <- 'Scenario_dataset_summary.xlsx'

# Load data output from the first (and only) sheet of the file. 
datamapping <-  (read.xlsx(f_datamap, sheet='Sheet 1'))

# Create data frame for regression output
regoutputfinal <- data.frame(samplesize = numeric(1),model = character(1),variable = character(1),slope = character(1),param = numeric(1),intercept = numeric(1),r2 = numeric(1),p_value = numeric(1),std_dev = numeric(1),iqr = numeric(1))          
# Specifying empty vectors in data.frame

# Loop through relevant files from scenario sets:
for (counter in 1:nrow(datamapping)) {
  
  # Load the csv-file generated in Preparation of data and read the scenario data:
  scens <- read.csv(paste("Scenario_dataset_detailed-",as.character(datamapping$V8[counter]),".csv",sep=""),stringsAsFactors=FALSE,strip.white=TRUE)
  
  # Load xlsx file including the models used and add model family variable. This is created as a separated xlsx file from the AR6 data.
  f_models <- 'Models.xlsx'
  models <-  (read.xlsx(f_models, sheet='Models'))
  scens <- merge(scens,models,by = "Model")
  
  # Rename the second column of scens to "concscen2". This need to be done in order to merge the model-scenarios between different files. 
  names(scens)[2] <- "concscen2"
  
  # Load xlsx file with model scenarios from variable AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|50.0th Percentile and the corresponding change in temperature in 2100 (50% MAGICC scores). The 50.0th percentile has been applied since there are no any climate scenario within the category C1 or C2 that have a higher value than 50.0. 
  # This is created as a separated xlsx file from the AR6 data.
  f_temp50 <- 'Temp50.xlsx'
  temp50 <-  (read.xlsx(f_temp50, sheet='Temp50'))
  names(temp50)[2] <- "Warm2100MAG50"
  scens <- merge(scens,temp50,by = "concscen2",all.x=TRUE,all.y=FALSE)
  
  # Section 3. Defining benchmarks and variables for plotting  =================================================================================
  
  # Selecting target benchmarks (mitigation variables). Note that not all variables 
  # are used in the tool, but included for test reasons. 
  benchdown <- c("Emissions|Kyoto Gases","Emissions|CO2|Energy and Industrial Processes","INT.emKyoto_gdp","INT.emCO2Elec_elecGen","INT.emCO2EI_PE","INT.emCO2EI_elecGen","INT.emCO2Transport_gdp","INT.emCO2EI_cement","INT.emCO2EI_steel","INT.emKyoto_m2","INT.emKyoto_finalenergy","INT.emCO2energysupply_SE","INT.emKyoto_PE","INT.emCO2energydemand_PE","Emissions|CO2|AFOLU","Emissions|CO2|Energy","Emissions|CO2|Energy|Demand","Emissions|CO2|Energy|Demand|Industry","Emissions|CO2|Energy|Demand|Residential and Commercial","Emissions|CO2|Energy|Demand|Transportation","Emissions|CO2|Energy|Demand|Transportation|Aviation","Emissions|CO2|Energy|Demand|Transportation|Rail","Emissions|CO2|Energy|Demand|Transportation|Road","Emissions|CO2|Energy|Supply","Emissions|CO2|Energy|Supply|Electricity","Emissions|CO2|Industrial Processes","Emissions|CO2|Energy|Demand|Industry|Cement","Emissions|CO2|Energy|Demand|Industry|Chemicals","Emissions|CO2|Energy|Demand|Transportation|Maritime","Emissions|CO2|Industrial Processes|Cement","Emissions|CO2|Industrial Processes|Chemicals","Emissions|CO2|Industrial Processes|Pulp and Paper","Emissions|CO2|Industrial Processes|Steel","Emissions|CO2|Energy|Demand|Industry|Pulp and Paper","Emissions|CO2|Energy|Demand|Industry|Steel",
                 "Median_peak_warming_MAGICCv7_5_3","Median_warming_in_2100_MAGICCv7_5_3")
  # These are defined in the input IPCC AR6 data from the first R-code, Preparation of data. The intensity target benchmarks (INT...) are defined in the first R-code, Preparation of data.   
  
  # Subset of data from benchdown
  benchdata <- subset(scens,variable %in% benchdown ,select = c(Category,concscen2,modelshort,Scenario,variable, slopeCA5, slopeCA10, slopeCA15, slopeCA20, slopeCA25, slopeCA30, Warm2100MAG50, Median_peak_warming_MAGICCv7_5_3, Median_warming_in_2100_MAGICCv7_5_3))
  
  # Creating a vector of the variables to use for benchmarks:  
  benchdownsub <- c("Emissions|Kyoto Gases","Emissions|CO2|Energy and Industrial Processes","INT.emKyoto_gdp","INT.emCO2Elec_elecGen","INT.emCO2EI_PE","INT.emCO2EI_elecGen","INT.emCO2Transport_gdp","INT.emCO2EI_cement","INT.emCO2EI_steel","INT.emKyoto_m2","INT.emKyoto_finalenergy","INT.emCO2energysupply_SE","INT.emKyoto_PE","INT.emCO2energydemand_PE","Emissions|CO2|AFOLU","Emissions|CO2|Energy","Emissions|CO2|Energy|Demand","Emissions|CO2|Energy|Demand|Industry","Emissions|CO2|Energy|Demand|Residential and Commercial","Emissions|CO2|Energy|Demand|Transportation","Emissions|CO2|Energy|Demand|Transportation|Aviation","Emissions|CO2|Energy|Demand|Transportation|Rail","Emissions|CO2|Energy|Demand|Transportation|Road","Emissions|CO2|Energy|Supply","Emissions|CO2|Energy|Supply|Electricity","Emissions|CO2|Industrial Processes","Emissions|CO2|Energy|Demand|Industry|Cement","Emissions|CO2|Energy|Demand|Industry|Chemicals","Emissions|CO2|Energy|Demand|Transportation|Maritime","Emissions|CO2|Industrial Processes|Cement","Emissions|CO2|Industrial Processes|Chemicals","Emissions|CO2|Industrial Processes|Pulp and Paper","Emissions|CO2|Industrial Processes|Steel","Emissions|CO2|Energy|Demand|Industry|Pulp and Paper","Emissions|CO2|Energy|Demand|Industry|Steel")
  
  
  # Re-name two variables in benchdata to facilitate the code:
  names(benchdata)[names(benchdata) %in% "Median_warming_in_2100_MAGICCv7_5_3"] <- "Warm2100MAG"
  names(benchdata)[names(benchdata) %in% "Median_peak_warming_MAGICCv7_5_3"] <- "PeakwarmMAG"
  
  # Reshape data for plotting
  benchdata <- melt(benchdata, id.vars = c("Category","concscen2","Scenario","modelshort","Warm2100MAG","PeakwarmMAG","Warm2100MAG50","variable"))
  names(benchdata)[length(names(benchdata))-1] <- "slope"
  slopes <- unique(benchdata$slope) # totalt 6 time horizons
  
  # Delete NA values from benchdata. This is necessary since not all mitigation scenarios are taking into consideration on all model-scenarios  
  benchdata <- benchdata[complete.cases(benchdata$value), ]
  benchdata <- benchdata[complete.cases(benchdata$Warm2100MAG50), ]
  benchdata <- benchdata[complete.cases(benchdata$Category), ]
  benchdata <- benchdata[complete.cases(benchdata$concscen2), ]
  benchdata <- benchdata[complete.cases(benchdata$modelshort), ]
  benchdata <- benchdata[complete.cases(benchdata$Warm2100MAG), ]
  benchdata <- benchdata[complete.cases(benchdata$PeakwarmMAG), ]
  benchdata <- benchdata[complete.cases(benchdata$variable), ]
  
  # Control reason: Count total NA values in the data frame
  total_na <- sum(is.na(benchdata))
  print(paste("Total NA values in benchdata: ", total_na))
  
  # Control reason: check NA values for each column
  na_each_column <- colSums(is.na(benchdata))
  print("NA values in each column: ")
  print(na_each_column)
  dim(benchdata)
  
  # Flipping the sign for target reductions to be positive "Value" refers to the 
  # slope value which is generated above, in the melt-function.
  benchdata$value <- benchdata$value*-1
  
  # Add overshoot level for plot labelling
  # The osratio is a measure of the intensity of the most extreme warming event (the peak) compared to the projected median warming by the end of the century. 
  # Dividing "Median peak warming" (PeakwarmMAG) by the "Median warming in 2100" (Warm2100MAG) measure the degree of overshoot compared to the expected warming in 2100.
  # The code block below does not have an implication of the results or output of the data/regression;  it only print the model-scenarios in the plots in different colours.  
  benchdata$osratio <- benchdata$PeakwarmMAG/benchdata$Warm2100MAG
  benchdata$oslabel <- "zerolow"
  benchdata$oslabel[benchdata$osratio > 1 & benchdata$osratio < 1.15] <- "mid"
  benchdata$oslabel[benchdata$osratio > 1.15] <- "high"
  
  #create shorter scenario/model name for plotting
  benchdata$scenshort <- substr(benchdata$Scenario,1,20)
  
  # Section 4. Plotting the data  =================================================================================
  
  #function for plotting equation and r2 on graph
  lm_eqn <- function(df){
    m <- lm(y ~ x, df);
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                     list(a = format(unname(coef(m)[1]), digits = 2),
                          b = format(unname(coef(m)[2]), digits = 2),
                          r2 = format(summary(m)$r.squared, digits = 3)))
    as.character(as.expression(eq));
  }
  
  #plot regressions by variable and horizon in a single PDF for each scenario set
  pdf(paste("Regressions_byvariables_byhorizon_temp50","constraint_set",datamapping$V8[counter],".pdf",sep="_"))
  for(i in 1:length(benchdownsub)){
    # Loop through benchdownsub
    for (j in 1:length(slopes)) {
      # Loop through slopes
      bdata <- subset(benchdata,variable %in% benchdown[i] & slope %in% slopes[j])
      bdata <- subset(benchdata,variable %in% benchdown[i] & slope %in% slopes[j])
      bdata2 <- subset(bdata,select = c("value","Warm2100MAG50"))
      names(bdata2) <- c("x","y")
      # Not all combinations of mitigation variables and slopes exists for all model-scenarios, and rows containing NA values has been deleted. The below code checks if bdata is empty or contains NA values, and only running rows with data. 
      if (nrow(bdata2) > 0) {
        print(paste("Data at i =", i, "which corresponds to variable =", benchdown[i], ", j =", j, "which corresponds to slope =", slopes[j]))
        templot <- ggplot(data = bdata) +
          geom_point(aes(x = value,y=Warm2100MAG50,color=oslabel),size = 2) +  #add color by model
          #data labels
          geom_text(aes(x = value,y=Warm2100MAG50,label = scenshort),size= 2) +
          geom_text(x=5,y=3.5, label = lm_eqn(bdata2), parse = TRUE) +
          ggtitle(paste(benchdown[i]," ",slopes[j])) +
          geom_smooth(method = lm, aes( x = value,y=Warm2100MAG50)) + 
          #stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE) +
          ylab("Projected warming in 2100") +
          xlab("Compound annual reduction - CAR (%)") +
          scale_x_continuous(limits = c(-5,10)) +
          scale_y_continuous(limits = c(0.5,5)) +
          theme(legend.position = "bottom")
        print(templot)
      }
    }
  }
  
  #close pdf
  dev.off()
  
  # Section 5. Create and store regression models  =================================================================================
  
  #initialize regression lists
  nrow_reg <- length(benchdownsub) * length(slopes)
  regoutput <- data.frame(
    samplesize = numeric(nrow_reg),
    variable = character(nrow_reg),
    slope = character(nrow_reg),
    param = numeric(nrow_reg),
    intercept = numeric(nrow_reg),
    r2 = numeric(nrow_reg),
    p_value = numeric(nrow_reg),
    std_dev = numeric(nrow_reg),
    iqr = numeric(nrow_reg)
  )
  regoutput$variable <- as.character(regoutput$variable)
  regoutput$slope <- as.character(regoutput$slope)
  row_counter = 1
  
  #plot regression diagnostic plots
  pdf(paste("Regression_diagnostics_","constraint_set",datamapping$V8[counter],".pdf"))
  for(i in 1:length(benchdownsub)){
    for (j in 1:length(slopes)) {
      print(paste(i, j, row_counter))
      bdata <- subset(benchdata,variable %in% benchdown[i] & slope %in% slopes[j])
      rownames(bdata) <- bdata$concscen2
      # Check if bdata is empty or contains NA values
      if (nrow(bdata) == 0) {
        print(paste("Data frame is empty at i =", i, "j =", j, "which corresponds to variable =", benchdown[i], "and slope =", slopes[j]))
      } else if (any(is.na(bdata$value), is.na(bdata$Warm2100MAG))) {
        print(paste("NA values found at i =", i, "j =", j, "which corresponds to variable =", benchdown[i], "and slope =", slopes[j]))
      } else {
        lmtemp <- lm(Warm2100MAG ~ value, data = bdata)
      }
      # Get the summary of the linear model
      lm_summary <- summary(lmtemp)
      
      # Extract the p-value of the predictor variable (value)
      p_value <- coef(lm_summary)["value", "Pr(>|t|)"]
      
      # Calculate the standard deviation of the residuals
      std_dev <- sd(lmtemp$residuals)
      
      # Calculate the IQR of the residuals
      iqr <- IQR(lmtemp$residuals)
      
      # Add these to your output dataframe
      regoutput$p_value[row_counter] <- p_value
      regoutput$std_dev[row_counter] <- std_dev
      regoutput$iqr[row_counter] <- iqr
      
      regoutput$model[row_counter] <- datamapping$V8[counter]
      regoutput$samplesize[row_counter] <- length(lmtemp$residuals)
      regoutput$variable[row_counter] <- benchdownsub[i]
      regoutput$slope[row_counter] <- levels(slopes)[j]
      regoutput$intercept[row_counter] <- lmtemp$coefficients[1]
      regoutput$param[row_counter] <- lmtemp$coefficients[2]
      regoutput$r2[row_counter] <- summary(lmtemp)$r.squared
      
      par(mfrow = c(2,2))
      print(paste("Data at i =", i, "which corresponds to variable =", benchdown[i], ", j =", j, "which corresponds to slope =", slopes[j]))
      plottemp <- plot(lmtemp,main = paste(benchdown[i]," ",slopes[j]))
      
      row_counter <- row_counter + 1
      
      
      
    }
  }
  
  
  #close pdf
  dev.off()
  
  # Print csv files with all model-scenarios in the scenario set:
  write.csv(scens, file = paste("Processed_Scenario_dataset_detailed-",as.character(datamapping$V8[counter]),".csv",sep=""), row.names = FALSE)
  
  regoutputfinal <- rbind(regoutputfinal,regoutput)
  #end loop
}

# Section 6. Save final output  =================================================================================

#remove initialization row
regoutputfinal <- subset(regoutputfinal,samplesize>0)

write.csv(regoutputfinal,"Regressions_Summary.csv",row.names = FALSE)

# ======================================= End of script =======================================


