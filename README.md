# ITR-regression
This repo contains an update of the TROPICS project to use AR6 scenarios for the regression modelling.
# R-code 

 This R-code is built on the the CDP-WWF temperature rating methodoloy, and the code TROPICS-scenario_prep.R available at https://github.com/CDPworldwide/TROPICS-regression/blob/master/TROPICS-scenario_prep.R. 

Running the R-code is the first step in the CDP-WWF temperature rating methodoloy. 

## About 

For this updated version 1.5, the R-code is updated with climate scenario data from the IPCC AR6 report. 

The code is divided into two parts:

> First, "CDP-WWF_ITR_preparation_of_data.R" is run to clean and prepare the data. The script creates output data for the next step. 

>  Second, "CDP-WWF_ITR_Regression.R" is run to create regressions and plots based on the AR6 scenarios, built on the output from AR6 Implementation_Preparation of data. These will be the benchmarks used in the later steps of the methodology. 

Visit https://github.com/WWF-Sweden/ITR-regression for the full documentation.

If you have any additional questions or comments send a mail to: ekonomi-finans@wwf.se

## Downloading the data 

In the first part of the R-code,"CDP-WWF_ITR_preparation_of_data.R", two input data files are used from the IPCC AR6 report. Both files can be downloaded from: https://data.ece.iiasa.ac.at/ar6/#/login

The first input file "AR6_Scenarios_Database_metadata_indicators_v1.0.xlsx" is available for download at: https://github.com/WWF-Sweden/ITR-regression/tree/main/Input_preparation_of_data

The second input file "“AR6_Scenarios_Database_World_v1.1.xlsx" is too large for Github and can be downloaded by following these steps: 

1.	Go to: https://data.ece.iiasa.ac.at/ar6/#/login
2.	When asked to login, choose option "login as guest". 
3.	Go to “Downloads” at the top of the page. 
4.	Scroll down until you find the list of datasets to download. Type in: “AR6_Scenarios_Database_World_v1.1” in the search bar. 
5.	When you click on the link you will be asked to type in your e-mail. The data set will be sent as a link to your email shortly and then downloaded as a zip file. 
6.	The zip file includes one excel file with metadata: “AR6_Scenarios_Database_metadata_indicators_v1.1.xlsx” and one CSV-file with world-data: “AR6_Scenarios_Database_World_v1.1.csv”. 
7.	In order to run the R-code with the World-data, open the CSV file with excel and save it as an xlsx-file. 
8.	Adjust the R-code to the right folder path where you have saved the two xlsx. input files. 
9.	Ready to run the code in R!

### Detailed information

To learn more about e.g. variables included or the different scenarios from the IPCC AR 6 report, detailed information is available at https://data.ece.iiasa.ac.at/ar6/#/docs



