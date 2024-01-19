# This code is built on the TROPICS-regression /Regression_plots_and_models_SR15TempScoring_vAndres.R (https://github.com/CDPworldwide/TROPICS-regression/blob/master/Regression_plots_and_models_SR15TempScoring_vAndres.R). 
# This is the second part of the R-code
# Updated for AR6 by WWF - Version 1 

######################################################################################################################.

#                                           Regression and plots of data 

######################################################################################################################.

# Description: This script creates regressions and plots of AR6 scenario data, built on the output from AR6 Implementation_Preparation of data. The code includes different sections: 
# Section 1. Preparation: install relevant packages and load libraries. 
# Section 2. Load data files generated in the first part of the R-code (Preparation of data) and xlsx created based on the underlying input  
# Section 3. Defining benchmarks and variables for plotting.
# Section 4. Plotting the data
# Section 5. Create and store regression models
# Section 6. Outlier analysis
# Section 7. Save final output

# Section 1. Preparation  =================================================================================

# clear working environment in R: 
rm(list=ls())

# Set the path for where files should be collected from. The path needs to be updated depending on where the files are stored: 
setwd("C:/Users/amanda.kihlman/R-test/AR6_Implementering/Input/ITR-reg-scenariodata_csv_sector_3_withoutBaseline")

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
# This has been updated since previous SR1.5 version of the code where the scenario sets needed to be 'deduplicated' before used in the R-code. See the methodology paper for additional information.
# This needs to be updated if normative filters are applied to structure the scenario sets.  
datamapping <-  (read.xlsx(f_datamap, sheet='Sheet 1'))

# Create data frame for regression output
regoutputfinal <- data.frame(samplesize = numeric(1),model = character(1),variable = character(1),slope = character(1),param = numeric(1),intercept = numeric(1),r2 = numeric(1))          
   # Specifying empty vectors in data.frame

# Loop through relevant files from scenario sets:
for (counter in 1:nrow(datamapping)) {

# Load the csv-file generated in Preparation of data and read the scenario data:
scens <- read.csv(paste("Scenario_dataset_detailed-",as.character(datamapping$V8[counter]),".csv",sep=""),stringsAsFactors=FALSE,strip.white=TRUE)

# Load xlsx file including the models used and add model family variable
  f_models <- 'Models.xlsx'
  models <-  (read.xlsx(f_models, sheet='Models'))
  scens <- merge(scens,models,by = "Model")

# Currently base case scenarios should be taking into consideration in the code and not removed. Therefore, the code-lines below are currently not considered.  
# Load xlsx file with baseline scenarios and remove baseline scenarios from the data, as they should not take part in modeling of mitigation targets
  f_baseline <- 'Baselines.xlsx'
  baselines <-  (read.xlsx(f_baseline, sheet='Baseline'))
  scens <- scens[!scens$Scenario %in% baselines$baseline,]
    # Rename the second column of scens to "concscen2". This need to be done in order to merge the model-scenarios between different files. 
  names(scens)[2] <- "concscen2"

  # Load xlsx file with model scenarios from variable AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|50.0th Percentile and the corresponding change in temperature in 2100 (50% MAGICC scores)
  f_temp50 <- 'Temp50.xlsx'
  temp50 <-  (read.xlsx(f_temp50, sheet='Temp50'))
  names(temp50)[2] <- "Warm2100MAG50"
  scens <- merge(scens,temp50,by = "concscen2",all.x=TRUE,all.y=FALSE)

# Section 3. Defining benchmarks and variables for plotting  =================================================================================

# Selecting target benchmarks - 24 - 5 intensity + 19 mitigation variables.
benchdown <- c("Emissions|Kyoto Gases","Emissions|CO2|Energy and Industrial Processes","INT.emKyoto_gdp","INT.emCO2Elec_elecGen","INT.emCO2EI_PE","INT.emCO2EI_elecGen","INT.emCO2Transport_gdp","Emissions|CO2|AFOLU","Emissions|CO2|Energy","Emissions|CO2|Energy|Demand","Emissions|CO2|Energy|Demand|Industry","Emissions|CO2|Energy|Demand|Residential and Commercial","Emissions|CO2|Energy|Demand|Transportation","Emissions|CO2|Energy|Demand|Transportation|Aviation","Emissions|CO2|Energy|Demand|Transportation|Rail","Emissions|CO2|Energy|Demand|Transportation|Road","Emissions|CO2|Energy|Supply","Emissions|CO2|Energy|Supply|Electricity","Emissions|CO2|Industrial Processes",
"Median_peak_warming_MAGICCv7_5_3","Median_warming_in_2100_MAGICCv7_5_3")
  # These are defined in the input IPCC AR6 data from the first R-code, Preparation of data.The intensity target benchmarks (INT...) are defined in the first R-code, Preparation of data.   

# Subset of data from benchdown
benchdata <- subset(scens,variable %in% benchdown ,select = c(Category,concscen2,modelshort,Scenario,variable, slope5, slope10, slope15, slope20, slope25, slope30, Warm2100MAG50, Median_peak_warming_MAGICCv7_5_3, Median_warming_in_2100_MAGICCv7_5_3))

# Creating a vector of the variables to use for benchmarks (24 in total):  
benchdownsub <- c("Emissions|Kyoto Gases","Emissions|CO2|Energy and Industrial Processes","INT.emKyoto_gdp","INT.emCO2Elec_elecGen","INT.emCO2EI_PE","INT.emCO2EI_elecGen","INT.emCO2Transport_gdp","Emissions|CO2|AFOLU","Emissions|CO2|Energy","Emissions|CO2|Energy|Demand","Emissions|CO2|Energy|Demand|Industry","Emissions|CO2|Energy|Demand|Residential and Commercial","Emissions|CO2|Energy|Demand|Transportation","Emissions|CO2|Energy|Demand|Transportation|Aviation","Emissions|CO2|Energy|Demand|Transportation|Rail","Emissions|CO2|Energy|Demand|Transportation|Road","Emissions|CO2|Energy|Supply","Emissions|CO2|Energy|Supply|Electricity","Emissions|CO2|Industrial Processes")


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

#### For control/test ####

print(colnames(benchdata))
print(nrow(benchdata))
unique(benchdata$variable)
unique(benchdata$slope)

# Count total NA values in the data frame
total_na <- sum(is.na(benchdata))
print(paste("Total NA values in benchdata: ", total_na))

# check NA values for each column
na_each_column <- colSums(is.na(benchdata))
print("NA values in each column: ")
print(na_each_column)
dim(benchdata)

#### End of control ####

# Flipping the sign for target reductions to be positive "Value" refers to the slope value which is generated above, in the melt-function.
benchdata$value <- benchdata$value*-1

# Add overshoot level for plot labelling
# The osratio is a measure of the intensity of the most extreme warming event (the peak) compared to the projected median warming by the end of the century. 
# It provides insight into how much more intense the peak warming could be relative to the general warming trend expected in 2100.
# The categorization into “zerolow,” “mid,” and “high” is a way to classify the data into different levels of peak warming relative to the warming in 2100.
# Dividing "Median peak warming" (PeakwarmMAG) by the "Median warming in 2100" (Warm2100MAG) measure the degree of overshoot compared to the expected warming in 2100.
# If the value is between 1 and 1.15, it is considered as a mid overshoot. If the value is higher than 1.15, it is considered high. 
# This is similar as to the SR1.5 data.
# The code does not have an implication of the results or output of the data. The only impact it has is that the model-scenarios in the printed plot is colour-coded. 
# In other words, if we do not want to priorities this, it is easy to delete this part of the code and colour-code feature of the plotting.  
benchdata$osratio <- benchdata$PeakwarmMAG/benchdata$Warm2100MAG
benchdata$oslabel <- "zerolow"
benchdata$oslabel[benchdata$osratio > 1 & benchdata$osratio < 1.15] <- "mid"
benchdata$oslabel[benchdata$osratio > 1.15] <- "high"

#create shorter scenario/model name for plotting
benchdata$scenshort <- substr(benchdata$Scenario,1,20)

# Print benchdata for further analysis if needed
write.csv(benchdata, file = "C:/Users/amanda.kihlman/R-test/AR6_Implementering/Input/ITR-reg-scenariodata_csv_sector_2/benchdata3.csv", row.names = FALSE)


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

# NOTE: This will be updated when we discussed if outliers should be removed, and the threshold used to assess outliers. 
# Outliers removal; concscen2s picked from visual inspection of Cook's D leverage
# outliers <- c("GCAM 4.2 SSP5-26","GCAM 4.2 SSP2-26","AIM/CGE 2.1 EMF33_tax_lo_none","AIM/CGE 2.1 EMF33_tax_lo_full","AIM/CGE 2.1 EMF33_tax_hi_none","AIM/CGE 2.1 EMF33_tax_hi_full")
# benchdata <- benchdata[!benchdata$concscen2 %in% outliers,]

# Remove baseline scenarios which have not been successfully removed in above code:
  baseline_removal <- c("IMAGE 3.2-SSP1-baseline","IMAGE 3.2-SSP2-baseline","IMAGE 3.2-SSP3-baseline","IMAGE 3.2-SSP4-baseline","IMAGE 3.2-SSP5-baseline")
  benchdata <- benchdata[!benchdata$concscen2 %in% baseline_removal,]
  scens <- scens[!scens$concscen2 %in% baseline_removal,]

#plot regressions by variable and horizon in a single PDF for each scenario set
pdf(paste("Regressions_byvariables_byhorizon_temp50","constraint_set",datamapping$V8[counter],".pdf",sep="_"))
for(i in 1:length(benchdownsub)){
    # Loop som går igenom varje benchdownsub - as selected above
    for (j in 1:length(slopes)) {
        # Loop som går igenom varje slopes - 6 st
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
    ylab("Warming in 2100, 50% chance") +
    xlab("Linear annual reduction rate (%)") +
    scale_x_continuous(limits = c(-5,7)) +
    scale_y_continuous(limits = c(0.5,5)) +
    theme(legend.position = "bottom")
  print(templot)
    }
  }
}

#close pdf
dev.off()

# For further analysis if needed 
#write.csv(bdata2, file = "C:/Users/amanda.kihlman/R-test/AR6_Implementering/Input/ITR-reg-scenariodata_csv_sector/bdata2_1.csv", row.names = FALSE)
#write.csv(bdata, file = "C:/Users/amanda.kihlman/R-test/AR6_Implementering/Input/ITR-reg-scenariodata_csv_sector/bdata_1.csv", row.names = FALSE)

# Section 5. Create and store regression models  =================================================================================

#initialize regression lists
nrow_reg <- length(benchdownsub) * length(slopes)
regoutput <- data.frame(
  samplesize = numeric(nrow_reg),
  variable = character(nrow_reg),
  slope = character(nrow_reg),
  param = numeric(nrow_reg),
  intercept = numeric(nrow_reg),
  r2 = numeric(nrow_reg)
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

# Section 6. Outlier analysis  =================================================================================

# Create an empty data frame to store potental outliers
outliers <- data.frame(
  model_scenario = character(),
  dataset = character(),
  variable = character(),
  slope = character(),
  residual = numeric(),
  leverage = numeric(),
  cooks_D = numeric(),
  stringsAsFactors = FALSE
)

# Loop through the scenario set, variable and slope
for (counter in 1:nrow(datamapping)) {
  for (i in 1:length(benchdownsub)) {
    for (j in 1:length(slopes)) {
      bdata <- subset(benchdata, variable %in% benchdown[i] & slope %in% slopes[j])
      bdata$value[is.na(bdata$value)] <- 0
      
      # Set row name
      rownames(bdata) <- bdata$concscen2
      
      # Apply linear regression
      lmtemp <- lm(Warm2100MAG ~ value, data = bdata)
      
      # Calculate residuals, leverage and Cook's D 
      std_residuals <- rstandard(lmtemp)
      leverage <- hatvalues(lmtemp)
      cooks_d <- cooks.distance(lmtemp)
      
      # Creating a data frame for the results, for the following thresholder: 
        # Residuals > 2
        # Cook's Leverage > 2*p/n
        # Cook's Distance > 4/n
      temp_outliers <- data.frame(
        model_scenario = rownames(bdata),
        dataset = rep(datamapping$V8[counter], nrow(bdata)),
        variable = rep(benchdownsub[i], nrow(bdata)),
        slope = rep(slopes[j], nrow(bdata)),
        # If one of the above thresholds are violated, 1 will appear in the output csv file.  
        residual = ifelse(abs(std_residuals) > 2, 1, 0),
        leverage = ifelse(leverage > 2*length(coef(lmtemp))/length(cooks_d), 1, 0),
        cooks_D = ifelse(cooks_d > 4/length(cooks_d), 1, 0),
        All_conditions = ifelse(abs(std_residuals) > 2 & leverage > 2*length(coef(lmtemp))/length(cooks_d) & cooks_d > 4/length(cooks_d), 1, 0),
        
        # Adding the absolut values for residuals, Cook's leverage och Cook's Distance
        abs_residual = abs(std_residuals),
        abs_leverage = abs(leverage),
        abs_cooks_D = abs(cooks_d),
        
        stringsAsFactors = FALSE
      )
      
      temp_outliers <- temp_outliers[temp_outliers$residual == 1 | temp_outliers$leverage == 1 | temp_outliers$cooks_D == 1, ]

      # Adding the results
      outliers <- rbind(outliers, temp_outliers)
    }
  }
}

# Section 7. Save final output  =================================================================================

#remove initialization row
regoutputfinal <- subset(regoutputfinal,samplesize>0)

write.csv(regoutputfinal,"Regressions_Summary.csv",row.names = FALSE)

#summarize regression output
sumreg <- ddply(regoutputfinal,.(variable,slope),summarize,size = median(samplesize),medregslope = median(param),regslopep10 = quantile(param,probs = 0.1),regslopep90 = quantile(param,probs = 0.9),medregint = median(intercept),regintp10 = quantile(intercept,probs = 0.1),regintp90 = quantile(intercept,probs = 0.9),medregr2 = median(r2),regr2p10 = quantile(r2,probs = 0.1),regr2p90 = quantile(r2,probs = 0.9))
write.csv(sumreg,"Regressions_crossmodel_summary.csv",row.names = FALSE)

#create rank by r2 for GHGs and GHG intensities
regoutwide <- dcast(regoutputfinal,model+slope ~ variable,value.var = "r2")
regoutwide15 <- subset(regoutwide,slope %in% "slope15")
regoutwide15$avgr215 <- rowMeans(regoutwide15[,names(regoutwide15) %in% c("Emissions|Kyoto Gases","INT.emKyoto_gdp","INT.emCO2EI_elecGen")])
regoutwide15 <- regoutwide15[order(-regoutwide15$avgr215),]
regoutwide15$rank15 <- NA
regoutwide15$rank15 <- 1:nrow(regoutwide15)
regoutwide30 <- subset(regoutwide,slope %in% "slope30")
regoutwide30$avgr230 <- rowMeans(regoutwide30[,names(regoutwide30) %in% c("Emissions|Kyoto Gases","INT.emKyoto_gdp","INT.emCO2EI_elecGen")])
regoutwide30 <- regoutwide30[order(-regoutwide30$avgr230),]
regoutwide30$rank30 <- NA
regoutwide30$rank30 <- 1:nrow(regoutwide30)
regoutwide2 <- regoutwide15[,names(regoutwide15) %in% c("model","avgr215","rank15")]
regoutwide3 <- regoutwide30[,names(regoutwide30) %in% c("model","avgr230","rank30")]
rankreg <- merge(regoutwide2,regoutwide3, by = c("model"))

#combine with data mappping
rankregfinal <- merge(rankreg,datamapping,by.x = "model",by.y = "V8")
names(rankregfinal) <- c("Scenario Set","Average R2, 15 year", "Rank R2, 15 year","Average R2, 30 year", "Rank R2, 30 year", "Number scenarios in set")
write.csv(rankregfinal,"Rank_r2_regressions.csv",row.names = FALSE)

#visualize R2 by sample size
x <- ggplot(data=subset(regoutputfinal,slope %in% "slope15")) + geom_point(aes(x=samplesize,y=r2,color=variable),size = 2) + theme_classic() + theme(legend.position = "bottom") 
ggsave("scatterplot_r2bysamplesize.pdf",x,width = 5, height = 4, units = "in",scale=1.8, dpi = 300,)

