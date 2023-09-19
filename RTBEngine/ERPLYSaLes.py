#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Sep 19 11:47:07 2023

@author: mutua
"""

import pandas as pd

MtamaTransactions = pd.read_csv("~/Documents/Work/Data/ERPLYSeptemberData.csv")
print(MtamaTransactions['TotalPrice'].sum())

ERPLYTransactions = pd.read_csv("~/Documents/Work/Data/SeptemberERPLY.csv")
ERPLYTransactions.loc[:, 'Amount Paid'] = ERPLYTransactions['Amount Paid'].str.replace(r'[,\s]+', '', regex=True)
ERPLYTransactions.loc[:, 'Amount Paid'] = ERPLYTransactions['Amount Paid'].astype(float).astype(int)

dd = ERPLYTransactions.loc[ERPLYTransactions['Date'] = '9/17/2023']]