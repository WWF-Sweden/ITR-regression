# -*- coding: utf-8 -*-
"""
Created on Tue Feb  6 10:54:16 2024
This script reads the output from the AR6 regression analysis and converts it to a JSON file.
The JSON file will then be used in the WWF ITR-tool
"""

import pandas as pd
import json
import os

# Set working directory to the location of the Excel input file with regressions
file_dir = os.path.dirname(os.path.abspath(__file__)) # change if necessary
os.chdir(file_dir)
print(os.getcwd())

# Load the Excel file
df = pd.read_excel('Regressions_Summary_AR6.xlsx') # change if necessary

df = df[(df['slope'].isin(['slopeCA5', 'slopeCA10', 'slopeCA30'])) & (df['model'] == 1)]

# Convert the DataFrame to JSON
json_data = df.to_json(orient='records')
parsed_json = json.loads(json_data)

# Write the JSON data to a file in a pretty format
with open('AR6_regression_model.json', 'w') as json_file:
    json_file.write(json.dumps(parsed_json, indent=4))