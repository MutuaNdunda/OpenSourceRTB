#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Aug 28 10:00:19 2023

@author: mutua
"""

import pandas as pd

def get_PP_text(ds, country, id):
    import re

    # We may want to change method_ploughing and method_ridging to character in the get_PP_recommendations function?
    ds['method_ploughing'] = ds['method_ploughing'].astype(str)
    ds['method_ridging'] = ds['method_ridging'].astype(str)

    pp_rec_text = pd.read_csv("pp_text.csv")

    if ds.loc[0, 'CP']:

        first_rec = pp_rec_text[(pp_rec_text['lang_id'] == id) & (pp_rec_text['rec_part'] == 1)]['rec_text'].values[0]
        if ds.loc[0, 'method_ploughing'] == "N/A":
            rec_plough = pp_rec_text[(pp_rec_text['lang_id'] == id) & (pp_rec_text['rec_part'] == 2)]['rec_text'].values[0]
        elif ds.loc[0, 'method_ploughing'] == "manual":
            rec_plough = pp_rec_text[(pp_rec_text['lang_id'] == id) & (pp_rec_text['rec_part'] == 3)]['rec_text'].values[0]
        elif ds.loc[0, 'method_ploughing'] == "tractor":
            rec_plough = pp_rec_text[(pp_rec_text['lang_id'] == id) & (pp_rec_text['rec_part'] == 4)]['rec_text'].values[0]

        if ds.loc[0, 'method_ridging'] == "N/A":
            rec_ridge = pp_rec_text[(pp_rec_text['lang_id'] == id) & (pp_rec_text['rec_part'] == 5)]['rec_text'].values[0]
            rec_ridge = " " + rec_ridge
        elif ds.loc[0, 'method_ridging'] == "manual":
            rec_ridge = pp_rec_text[(pp_rec_text['lang_id'] == id) & (pp_rec_text['rec_part'] == 6)]['rec_text'].values[0]
            rec_ridge = " " + rec_ridge
        elif ds.loc[0, 'method_ridging'] == "tractor":
            rec_ridge = pp_rec_text[(pp_rec_text['lang_id'] == id) & (pp_rec_text['rec_part'] == 7)]['rec_text'].values[0]
            rec_ridge = " " + rec_ridge

        last_rec = pp_rec_text[(pp_rec_text['lang_id'] == id) & (pp_rec_text['rec_part'] == 8)]['rec_text'].values[0]
        rec = first_rec + rec_plough + rec_ridge + last_rec

    else:

        recT = ""

        if ds.loc[0, 'ploughing'] and ds.loc[0, 'ridging']:
            recT = "".join([
                pp_rec_text[(pp_rec_text['lang_id'] == id) & (pp_rec_text['rec_part'] == 9)]['rec_text'].values[0],
                ds.loc[0, 'method_ploughing'],
                pp_rec_text[(pp_rec_text['lang_id'] == id) & (pp_rec_text['rec_part'] == 10)]['rec_text'].values[0],
                ds.loc[0, 'method_ridging'],
                pp_rec_text[(pp_rec_text['lang_id'] == id) & (pp_rec_text['rec_part'] == 11)]['rec_text'].values[0]
            ])

        if ds.loc[0, 'ploughing'] and not ds.loc[0, 'ridging']:
            recT = "".join([
                pp_rec_text[(pp_rec_text['lang_id'] == id) & (pp_rec_text['rec_part'] == 9)]['rec_text'].values[0],
                ds.loc[0, 'method_ploughing'],
                pp_rec_text[(pp_rec_text['lang_id'] == id) & (pp_rec_text['rec_part'] == 12)]['rec_text'].values[0]
            ])

        # Handling third case
        if not ds.loc[0, 'ploughing'] and ds.loc[0, 'ridging']:
            recT_first = pp_rec_text[(pp_rec_text['lang_id'] == id) & (pp_rec_text['rec_part'] == 13)]['rec_text'].values[0]

            if ds.loc[0, 'method_ridging'] == "manual":
                recT_sec = pp_rec_text[(pp_rec_text['lang_id'] == id) & (pp_rec_text['rec_part'] == 14)]['rec_text'].values[0]
            else:
                recT_sec = pp_rec_text[(pp_rec_text['lang_id'] == id) & (pp_rec_text['rec_part'] == 15)]['rec_text'].values[0]
            
            recT_third = pp_rec_text[(pp_rec_text['lang_id'] == id) & (pp_rec_text['rec_part'] == 16)]['rec_text'].values[0]

            recT = recT_first + recT_sec + recT_third

        if not ds.loc[0, 'ploughing'] and not ds.loc[0, 'ridging']:
            recT = pp_rec_text[(pp_rec_text['lang_id'] == id) & (pp_rec_text['rec_part'] == 17)]['rec_text'].values[0]

        currency = "NGN" if country == "NG" else "TZS"
        changeTC = ds.loc[0, 'dTC']
        dTC = format(abs(ds.loc[0, 'dTC']), ".3f")
        dNR = format(ds.loc[0, 'dNR'], ".3f")
        dRP = format(ds.loc[0, 'dRP'], ".2f")

        if dTC == "0.000":
            recC = pp_rec_text[(pp_rec_text['lang_id'] == id) & (pp_rec_text['rec_part'] == 18)]['rec_text'].values[0]
        else:
            # Change decrease increase for swahili language
            if id == 2:
                decrease = "itapunguza"
                increase = "itaongeza"
            else:
                decrease = pp_rec_text[(pp_rec_text['lang_id'] == id) & (pp_rec_text['rec_part'] == 24)]['rec_text'].values[0]
                increase = pp_rec_text[(pp_rec_text['lang_id'] == id) & (pp_rec_text['rec_part'] == 25)]['rec_text'].values[0]

            recC = "".join([
                pp_rec_text[(pp_rec_text['lang_id'] == id) & (pp_rec_text['rec_part'] == 19)]['rec_text'].values[0],
                decrease if changeTC < 0 else increase,
                pp_rec_text[(pp_rec_text['lang_id'] == id) & (pp_rec_text['rec_part'] == 20)]['rec_text'].values[0],
                currency,
                " ",
                dTC,
                ". "
            ])

        if dRP == "0.00" and dNR == "0.000":
            recP = pp_rec_text[(pp_rec_text['lang_id'] == id) & (pp_rec_text['rec_part'] == 21)]['rec_text'].values[0]

        if dRP == "0.00" and dNR > "0.000":
            recP = "".join([
                pp_rec_text[(pp_rec_text['lang_id'] == id) & (pp_rec_text['rec_part'] == 22)]['rec_text'].values[0],
                currency,
                " ",
                dNR,
                "."
            ])

        if dRP != "0.00" and dNR == "0.000":
            recP = "".join([
                pp_rec_text[(pp_rec_text['lang_id'] == id) & (pp_rec_text['rec_part'] == 23)]['rec_text'].values[0],
                decrease if ds.loc[0, 'TC'] < 0 else increase,
                pp_rec_text[(pp_rec_text['lang_id'] == id) & (pp_rec_text['rec_part'] == 26)]['rec_text'].values[0],
                dRP,
                pp_rec_text[(pp_rec_text['lang_id'] == id) & (pp_rec_text['rec_part'] == 27)]['rec_text'].values[0],
                ", ",
                pp_rec_text[(pp_rec_text['lang_id'] == id) & (pp_rec_text['rec_part'] == 29)]['rec_text'].values[0],
                pp_rec_text[(pp_rec_text['lang_id'] == id) & (pp_rec_text['rec_part'] == 28)]['rec_text'].values[0]
            ])

        if dRP != "0.00" and dNR != "0.000":
            recP = "".join([
                pp_rec_text[(pp_rec_text['lang_id'] == id) & (pp_rec_text['rec_part'] == 23)]['rec_text'].values[0],
                decrease if ds.loc[0, 'TC'] < 0 else increase,
                pp_rec_text[(pp_rec_text['lang_id'] == id) & (pp_rec_text['rec_part'] == 26)]['rec_text'].values[0],
                dRP,
                pp_rec_text[(pp_rec_text['lang_id'] == id) & (pp_rec_text['rec_part'] == 27)]['rec_text'].values[0],
                decrease if ds.loc[0, 'TC'] < 0 else increase,
                pp_rec_text[(pp_rec_text['lang_id'] == id) & (pp_rec_text['rec_part'] == 30)]['rec_text'].values[0],
                pp_rec_text[(pp_rec_text['lang_id'] == id) & (pp_rec_text['rec_part'] == 31)]['rec_text'].values[0],
                currency,
                " ",
                dNR
            ])

        rec = recT + recC + recP

    # TODO: This only provides the minimal information to return to the user. We may consider adding the following information:
    # 1. Beware that planting on flat may not be advisable in your specific conditions. You should ridge if the land is sometimes very wet (water-logging problems), if controlling weed is very challenging, if the soil is very clayey, or if you plan to harvest during the dry season.
    # 2. We currently do not consider costs and benefits of harrowing - we have not investigated this.
    # 3. Explicit reasons underlying recommendations (driven by cost-saving or revenue increase).
    # 4. Our selection of the best option may differ from the one by the farmer. A farmer may be willing to choose an option that has a lower net revenue change than the recommended, but also a lower cost.
    # 5. Possible issues with the input data - especially if the user provides unrealistic prices.
    rec = re.sub(r'\s+', ' ', rec)
    return rec
