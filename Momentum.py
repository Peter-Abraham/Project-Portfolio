#A momentum based quantitative trading strategy that identifies high quality momentum stocks and recommends the number of shares to buy based on the input portfilio amount.

import numpy
import pandas as pd
import requests
import xlsxwriter
import math
from scipy import stats
import json

input_data = '/Users/peterabraham/Downloads/sp_500_stocks.csv'
stocks = pd.read_csv(input_data)

from secrety import IEX_CLOUD_API_TOKEN


symbol = 'SMID'

api_url = f'https://cloud.iexapis.com/stable/stock/{symbol}/stats?token={IEX_CLOUD_API_TOKEN}'
data = requests.get(api_url).json()
data
data['year1ChangePercent']

# Function sourced from 
# https://stackoverflow.com/questions/312443/how-do-you-split-a-list-into-evenly-sized-chunks
def chunks(lst, n):
    """Yield successive n-sized chunks from lst."""
    for i in range(0, len(lst), n):
        yield lst[i:i + n]   
        
symbol_groups = list(chunks(stocks['Ticker'], 100))
symbol_strings = []
for i in range(0, len(symbol_groups)):
    symbol_strings.append(','.join(symbol_groups[i]))
#     print(symbol_strings[i])

my_columns = ['Ticker', 'Price', 'One-Year Price Return', 'Number of Shares to Buy']


final_dataframe = pd.DataFrame(columns = my_columns)

for symbol_string in symbol_strings:
#     print(symbol_strings)
    batch_api_call_url = f'https://cloud.iexapis.com/stable/stock/market/batch/?types=stats,quote&symbols={symbol_string}&token={IEX_CLOUD_API_TOKEN}'
    data = requests.get(batch_api_call_url).json()
    for symbol in symbol_string.split(','):
        final_dataframe = final_dataframe.append(
                                        pd.Series([symbol, 
                                                   data[symbol]['quote']['latestPrice'],
                                                   data[symbol]['stats']['year1ChangePercent'],
                                                   'N/A'
                                                   ], 
                                                  index = my_columns), 
                                        ignore_index = True)
        
    
final_dataframe

final_dataframe.sort_values('One-Year Price Return', ascending = False, inplace = True)
final_dataframe = final_dataframe[:51]
final_dataframe.reset_index(drop = True, inplace = True)
final_dataframe

def portfolio_input():
    global portfolio_size
    portfolio_size = input("Enter the value of your portfolio:")

    try:
        val = float(portfolio_size)
    except ValueError:
        print("That's not a number! \n Try again:")
        portfolio_size = input("Enter the value of your portfolio:")

portfolio_input()
print(portfolio_size)

position_size = float(portfolio_size) / len(final_dataframe.index)
for i in range(0, len(final_dataframe['Ticker'])):
    final_dataframe.loc[i, 'Number of Shares to Buy'] = math.floor(position_size / final_dataframe['Price'][i])
final_dataframe

hqm_columns = [
                'Ticker', 
                'Price', 
                'Number of Shares to Buy', 
                'One-Year Price Return', 
                'One-Year Return Percentile',
                'Six-Month Price Return',
                'Six-Month Return Percentile',
                'Three-Month Price Return',
                'Three-Month Return Percentile',
                'One-Month Price Return',
                'One-Month Return Percentile',
                'HQM Score'
                ]

hqm_dataframe = pd.DataFrame(columns=hqm_columns)
convert_none = lambda x : 0 if x is None else x

for symbol_string in symbol_strings:
    batch_api_call_url = f'https://cloud.iexapis.com/stable/stock/market/batch?symbols={symbol_string}&types=price,stats&token={IEX_CLOUD_API_TOKEN}'
    data = requests.get(batch_api_call_url).json()
    
    for symbol in symbol_string.split(','):
        hqm_dataframe = hqm_dataframe.append(
            pd.Series(
                [
                    symbol,
                    data[symbol]['price'],
                    'N/A',
                    convert_none(data[symbol]['stats']['year1ChangePercent']),
                    'N/A',
                    convert_none(data[symbol]['stats']['month6ChangePercent']),
                    'N/A',
                    convert_none(data[symbol]['stats']['month3ChangePercent']),
                    'N/A',
                    convert_none(data[symbol]['stats']['month1ChangePercent']),
                    'N/A',
                    'N/A'
                    
                    
                ],
                index = hqm_columns
            ),
            ignore_index=True
        )



time_periods = [
                'One-Year',
                'Six-Month',
                'Three-Month',
                'One-Month'
                ]

for row in hqm_dataframe.index:
    for time_period in time_periods:
        hqm_dataframe.loc[row, f'{time_period} Return Percentile'] = stats.percentileofscore(hqm_dataframe[f'{time_period} Price Return'], hqm_dataframe.loc[row, f'{time_period} Price Return'])/100

# Print each percentile score to make sure it was calculated properly
for time_period in time_periods:
    print(hqm_dataframe[f'{time_period} Return Percentile'])

#Print the entire DataFrame    
hqm_dataframe



from statistics import mean

for row in hqm_dataframe.index:
    momentum_percentiles = []
    for time_period in time_periods:
        momentum_percentiles.append(hqm_dataframe.loc[row, f'{time_period} Return Percentile'])
    hqm_dataframe.loc[row, 'HQM Score'] = mean(momentum_percentiles)

hqm_dataframe.sort_values(by = 'HQM Score', ascending = False)
hqm_dataframe = hqm_dataframe[:51]

portfolio_input()

position_size = float(portfolio_size) / len(hqm_dataframe.index)
for i in range(0, len(hqm_dataframe['Ticker'])-1):
    hqm_dataframe.loc[i, 'Number of Shares to Buy'] = math.floor(position_size / hqm_dataframe['Price'][i])
with pd.option_context('display.max_rows', None, 'display.max_columns', None):

    print(hqm_dataframe)

#hqm_dataframe.to_csv('Momentum.txt', sep='\t')




