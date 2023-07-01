import pandas as pd
import numpy as np

# Conversion of areaUnits into hectares
def areaHA_function(areaUnits, area):
    if areaUnits == "ha":
        areaHa = area
    elif areaUnits == "acre":
        areaHa = area / 2.47105
    else:
        areaHa = area / 10000
    return areaHa

def root_conversion(country, cassPD="roots", cassUP=None, cassUW=None, cassUP_m1=None, cassUP_m2=None, cassUP_p1=None, cassUP_p2=None):
    conversion_factors = {
        "roots": 1,
        "chips": 3,
        "flour": 3.2,
        "gari": 3.5
    }
    
    cassUP_defaults = {
        "roots": {"NG": 12000, "TZ": 180000},
        "chips": {"NG": 36000, "TZ": 540000},
        "flour": {"NG": 38400, "TZ": 576000},
        "gari": {"NG": 42000, "TZ": 630000}
    }
    
    conversion_factor = conversion_factors.get(cassPD, 1)
    
    if cassUW in (0, None):
        cassUW = 1000
    
    if cassUP is None and country in cassUP_defaults.get(cassPD, {}):
        cassUP = cassUP_defaults[cassPD][country]
        cassUW = 1000
    
    rootUP = cassUP / cassUW / conversion_factor * 1000

    try:
        rootUP_m1 = cassUP_m1 / cassUW / conversion_factor * 1000
    except (ZeroDivisionError, TypeError):
        rootUP_m1 = 0

    try:
        rootUP_m2 = cassUP_m2 / cassUW / conversion_factor * 1000
    except (ZeroDivisionError, TypeError):
        rootUP_m2 = 0

    try:
        rootUP_p1 = cassUP_p1 / cassUW / conversion_factor * 1000
    except (ZeroDivisionError, TypeError):
        rootUP_p1 = 0

    try:
        rootUP_p2 = cassUP_p2 / cassUW / conversion_factor * 1000
    except (ZeroDivisionError, TypeError):
        rootUP_p2 = 0
    
    rootUP_All = [rootUP, rootUP_m1, rootUP_m2, rootUP_p1, rootUP_p2]
    
    return rootUP_All

#Testing 
areaUnits = 'acre'
cost_LMO_areaBasis = 'areaUnit'
tractor_plough = True
tractor_ridger = False
cost_manual_ploughing = 0
cost_manual_ridging = None
cost_tractor_ridging = None
cost_tractor_ploughing = 5600
cost_weeding1 = 6700
cost_weeding2 = 5600
areaHa = 4500
country = 'TZ'

# Creation of costLMO dataframe
def cost_lmo_creation(areaUnits, cost_LMO_areaBasis, cost_manual_ploughing, cost_manual_ridging, cost_tractor_ploughing, cost_tractor_ridging, cost_weeding1, cost_weeding2, areaHa, country):
    costLMO = pd.DataFrame({
        'operation': ['ploughing', 'ridging', 'ploughing', 'ridging', 'weeding1', 'weeding2'],
        'method': ['manual', 'manual', 'tractor', 'tractor', np.nan, np.nan],
        'cost': [cost_manual_ploughing, cost_manual_ridging, cost_tractor_ploughing, cost_tractor_ridging, cost_weeding1, cost_weeding2],
        'area': np.where(cost_LMO_areaBasis == 'areaField', areaHa, np.where(areaUnits == 'acre', 0.404686, np.where(areaUnits == 'ha', 1, 0.0001)))
    })
    
    costLMO['costHa'] = costLMO['cost'] / costLMO['area']
    costLMO = costLMO.drop(['area', 'cost'], axis=1)

    # Add default values for LMO operations if missing
    default_values = {
        ('NG', 'ploughing', 'manual'): 17000 * 2.47105,
        ('NG', 'ridging', 'manual'): 12000 * 2.47105,
        ('NG', 'ploughing', 'tractor'): 6000 * 2.47105,
        ('NG', 'ridging', 'tractor'): 6000 * 2.47105,
        ('NG', 'weeding1', np.nan): 12500 * 2.47105,
        ('NG', 'weeding2', np.nan): 12500 * 2.47105,
        ('TZ', 'ploughing', 'manual'): 432433.9,
        ('TZ', 'ridging', 'manual'): 555986.2,
        ('TZ', 'ploughing', 'tractor'): 370657.5,
        ('TZ', 'ridging', 'tractor'): 284170.9,
        ('TZ', 'weeding1', np.nan): 100000 * 2.47105,
        ('TZ', 'weeding2', np.nan): 850000 * 2.47105
    }

    for key, value in default_values.items():
        country_val, operation_val, method_val = key
        if pd.isna(costLMO.loc[(costLMO['operation'] == operation_val) & (costLMO['method'] == method_val), 'costHa'].iloc[0]) and country == country_val:
            costLMO.loc[(costLMO['operation'] == operation_val) & (costLMO['method'] == method_val), 'costHa'] = value

    return costLMO