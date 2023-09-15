import pandas as pd
import io
import itertools

# Define the dataset
data = """Region,Area,Duka_Name
Coast,CTA01,Kikoneni
Coast,CTA01,Kwale
Coast,CTA01,Mamba
Coast,CTA01,Mkongani
Coast,CTA01,Msambweni
Coast,CTA02,Dzistoni
Coast,CTA02,Kakanjuni
Coast,CTA02,Kidutani
Coast,CTA02,Maereni
Coast,CTA02,Matanoma"""

# Read the dataset into a DataFrame
all_duka = pd.read_csv(io.StringIO(data))

# Define the years and months
years = [2019, 2020, 2021, 2023]
months = ['January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December']
channels = ['Online', 'Duka', 'Credit']

# Create a list of all possible combinations of Year, Month, and Channel
combinations = list(itertools.product(years, months, channels))

# Create a DataFrame to store the combinations
combinations_df = pd.DataFrame(combinations, columns=['Year', 'Month', 'Channel'])

# Merge the combinations with the "all_duka" DataFrame using a Cartesian join (cross join)
result_df = pd.merge(all_duka.assign(key=1), combinations_df.assign(key=1), on='key').drop(columns='key')
result_df['main_id'] = result_df['Duka_Name'] + result_df['Year'].astype(str) + result_df['Month'] + result_df['Channel']


# Display the resulting DataFrame
print(result_df)
