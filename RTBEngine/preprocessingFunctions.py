import pandas as pd
import numpy as np

"""
The areaHA_function is a function that converts an area from one unit (acres or hectares) to another unit (hectares). It takes two parameters as input:

areaUnits: This parameter represents the unit of the input area and is expected to be a string. It can take three possible values:

"ha": Represents hectares.
"acre": Represents acres.
Any other value: Assumes that the input is in square meters.
area: This parameter represents the numeric value of the area to be converted.

The function performs the following conversions based on the areaUnits parameter:

1)If areaUnits is "ha" (hectares), it assumes that the input area is already in hectares, and it returns the input area value as is.

2)If areaUnits is "acre" (acres), it assumes that the input area is in acres and converts it to hectares by dividing it by the conversion factor of 2.47105 (since 1 acre is approximately 2.47105 hectares).

3)If areaUnits is any other value, it assumes that the input area is in square meters and converts it to hectares by dividing it by 10,000 (since 1 hectare is 10,000 square meters).

4)Finally, the function returns the converted area in hectares (areaHa).
"""

def areaHA_function(areaUnits, area):
    if areaUnits == "ha":
        areaHa = area
    elif areaUnits == "acre":
        areaHa = area / 2.47105
    else:
        areaHa = area / 10000
    return areaHa


"""
This function, root_conversion, is a function that calculates and returns conversion values related to cassava processing. It takes several parameters as input, but most of them have default values, making them optional. Here's a breakdown of what this function does:

country: A string parameter representing the country for which the conversion is being calculated.

cassPD: A string parameter representing the type of cassava product being processed. It has a default value of "roots."

cassUP: A numeric parameter representing the quantity of cassava product (e.g., cassava roots, chips, flour, or gari) being processed. It has a default value of None.

cassUW: A numeric parameter representing the weight of the cassava product in kilograms. It also has a default value of None.

cassUP_m1, cassUP_m2, cassUP_p1, cassUP_p2: Numeric parameters representing quantities of cassava products at different stages of processing. They have default values of None.

The function performs the following steps:

1)It defines conversion factors for different types of cassava products (e.g., roots, chips, flour, gari) in a dictionary called conversion_factors.

2)It defines default quantities of cassava products for Nigeria (NG) and Tanzania (TZ) in a dictionary called cassUP_defaults.

3)It calculates the conversion factor based on the specified cassPD. If cassPD is not specified or not found in the dictionary, the default conversion factor is set to 1.

4)It checks if cassUW is either 0 or None, and if so, it sets it to 1000 (assuming a default weight of 1000 kilograms).

5)It checks if cassUP is None and if the country is in the cassUP_defaults dictionary for the specified cassPD. If both conditions are met, it sets cassUP to the default value specified for the country.

6)It calculates rootUP, which represents the conversion of cassava product quantity to units per kilogram, taking into account the conversion factor and weight.

7)It calculates similar conversions for rootUP_m1, rootUP_m2, rootUP_p1, and rootUP_p2, which represent quantities at different processing stages.

8)It stores these calculated values in a list called rootUP_All.

Finally, it returns the list of conversion values.
"""

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


'''
cost_lmo_creation fucntion, is designed to create a DataFrame that contains cost-related information for various agricultural operations in different countries. Here's a breakdown of what this function does:

It takes several parameters as input, including areaUnits, cost_LMO_areaBasis, cost_manual_ploughing, cost_manual_ridging, cost_tractor_ploughing, cost_tractor_ridging, cost_weeding1, cost_weeding2, areaHa, and country.

It creates a Pandas DataFrame called costLMO with the following columns:

'operation': This column contains operation names like 'ploughing,' 'ridging,' 'weeding1,' etc.
'method': This column contains method names like 'manual' and 'tractor.' It also contains NaN values for some rows.
'cost': This column contains cost values for each operation and method.
'area': This column calculates the area based on areaUnits and cost_LMO_areaBasis parameters. The calculation depends on whether areaUnits is 'acre' or 'ha' and whether cost_LMO_areaBasis is 'areaField.'
It calculates a new column, 'costHa,' by dividing the 'cost' column by the 'area' column. This represents the cost per hectare (or acre) for each operation and method.

It drops the 'area' and 'cost' columns from the DataFrame, as they are no longer needed.

It adds a new column, 'country,' to the DataFrame and sets its values to the country parameter passed to the function.

It defines a dictionary called default_values, which contains default cost values for different combinations of country, operation, and method. These default values are used in case there is missing cost data in the DataFrame.

It iterates through the default_values dictionary and checks if there are any missing cost values in the costLMO DataFrame for the specified country, operation, and method. If missing values are found, the function updates them with the corresponding default values.

Finally, it returns the modified costLMO DataFrame, which now includes default cost values for missing entries.

In summary, this function is used to create a DataFrame of agricultural operation costs per hectare (or acre) while handling missing cost data by applying default values based on country, operation, and method.
'''
def cost_lmo_creation(areaUnits, cost_LMO_areaBasis, cost_manual_ploughing, cost_manual_ridging, cost_tractor_ploughing, cost_tractor_ridging, cost_weeding1, cost_weeding2, areaHa, country):
    costLMO = pd.DataFrame({
        'operation': ['ploughing', 'ridging', 'ploughing', 'ridging', 'weeding1', 'weeding2'],
        'method': ['manual', 'manual', 'tractor', 'tractor', np.nan, np.nan],
        'cost': [cost_manual_ploughing, cost_manual_ridging, cost_tractor_ploughing, cost_tractor_ridging, cost_weeding1, cost_weeding2],
        'area': np.where(cost_LMO_areaBasis == 'areaField', areaHa, np.where(areaUnits == 'acre', 0.404686, np.where(areaUnits == 'ha', 1, 0.0001)))
    })

    costLMO['costHa'] = costLMO['cost'] / costLMO['area']
    costLMO = costLMO.drop(['area', 'cost'], axis=1)
    costLMO['country'] = country

    # Add default values for LMO operations if missing
    default_values = {
        ('NG', 'ploughing', 'manual'): 17000 * 2.47105,
        ('NG', 'ridging', 'manual'): 12000 * 2.47105,
        ('NG', 'ploughing', 'tractor'): 6000 * 2.47105,
        ('NG', 'ridging', 'tractor'): 6000 * 2.47105,
        ('NG', 'weeding1', None): 12500 * 2.47105,
        ('NG', 'weeding2', None): 12500 * 2.47105,
        ('TZ', 'ploughing', 'manual'): 432433.9,
        ('TZ', 'ridging', 'manual'): 555986.2,
        ('TZ', 'ploughing', 'tractor'): 370657.5,
        ('TZ', 'ridging', 'tractor'): 284170.9,
        ('TZ', 'weeding1', None): 100000 * 2.47105,
        ('TZ', 'weeding2', None): 850000 * 2.47105
    }

    for key, value in default_values.items():
        country_val, operation_val, method_val = key
        matching_rows = costLMO.loc[(costLMO['operation'] == operation_val) & (costLMO['method'].isna()) & (costLMO['country'] == country_val), 'costHa']
        
        if not matching_rows.empty:
            if pd.isna(matching_rows.iloc[0]):
                costLMO.loc[(costLMO['operation'] == operation_val) & (
                    costLMO['method'].isna()) & (costLMO['country'] == country_val), 'costHa'] = value


    return costLMO

'''
Fertilizer_input fucntion, is designed to create a Pandas DataFrame containing information about fertilizer prices and nutrient contents based on input values for urea, NPK (Nitrogen, Phosphorus, Potassium), and DAP (Diammonium Phosphate) prices. Here's a breakdown of what this function does:

urea_price, npk_price, dap_price: These parameters represent the prices of urea, NPK, and DAP fertilizers, respectively.

country: This parameter represents the country for which fertilizer information is being calculated.

The function performs the following steps:

It first checks if the country parameter is either "TZ" (Tanzania) or "NG" (Nigeria). If not, it returns a message indicating that the country must be "TZ" or "NG."

If the country is Nigeria ("NG"), the function:

Converts the input fertilizer prices (urea_price and npk_price) into numeric values and calculates the price per kilogram.
Adds default prices for urea and NPK fertilizers if the input prices are missing (None) or zero.
Creates a dictionary (fertilizers) containing fertilizer type, nutrient content (N, P, K), and price per kilogram.
Returns a Pandas DataFrame using this dictionary.
If the country is Tanzania ("TZ"), the function:

Converts the input fertilizer prices (urea_price, dap_price, and npk_price) into numeric values and calculates the price per kilogram.
Adds default prices for urea, DAP, and NPK fertilizers if the input prices are missing (None) or zero.
Creates a dictionary (fertilizers) containing fertilizer type, nutrient content (N, P, K), and price per kilogram.
Returns a Pandas DataFrame using this dictionary.
If the country is neither "TZ" nor "NG," the function prints a message indicating that the input country should be checked.
'''
def fertilizer_input(urea_price, npk_price, dap_price, country):
    # Check if country is TZ or NG
    if country not in ["TZ", "NG"]:
        return "Country must be TZ or NG"

    # If the country is Nigeria
    if country == "NG":
        # Convert the inputs into numeric
        price = [float(urea_price), float(npk_price)]
        # Get price per KG
        price = [p / 50 for p in price]

        # Add default prices for urea.
        if price[0] is None or price[0] == 0:
            price[0] = 150

        # Add default prices for npk fertilizer.
        if price[1] is None or price[1] == 0:
            price[1] = 170

        # Creating the fertilizer DataFrame
        fertilizers = {
            'type': ["urea", "NPK"],
            'N_cont': [0.46, 0.15],
            'P_cont': [0, 0.07],
            'K_cont': [0, 0.125],
            'price': price
        }

        return pd.DataFrame(fertilizers)

    elif country == "TZ":
        # Convert the inputs into numeric
        price = [float(urea_price), float(dap_price), float(npk_price)]
        # Get price per KG
        price = [p / 50 for p in price]

        # Add default prices for urea.
        if price[0] is None or price[0] == 0:
            price[0] = 1160

        # Add default prices for DAP.
        if price[1] is None or price[1] == 0:
            price[1] = 1260

        # Add default prices for npk fertilizer.
        if price[2] is None or price[2] == 0:
            price[2] = 1200

        # Creating the fertilizer DataFrame
        fertilizers = {
            'type': ["urea", "DAP", "NPK"],
            'N_cont': [0.46, 0.18, 0.17],
            'P_cont': [0, 0.46, 0.083],
            'K_cont': [0, 0, 0.15],
            'price': price
        }

        return pd.DataFrame(fertilizers)

    else:
        print("\nKindly check the input for country\n")

'''
FCY preprocesing, is designed to preprocess a farm's current yield (FCY) based on the provided area and area units. Here's a breakdown of what this function does:

FCY: This parameter represents the farm's current yield, which is a numeric value.

areaUnits: This parameter represents the units in which the farm's area is provided, either "ha" (hectares) or "acre."

area: This parameter represents the farm's area, typically in hectares or acres, depending on the areaUnits parameter.

The function performs the following steps:

It converts the FCY parameter to a floating-point number, ensuring that it's numeric.

It checks if FCY is either None or equal to 0 and whether the areaUnits are in hectares ("ha"). If both conditions are met, it sets a default value for FCY based on the formula: 11.25 * area. This is a default yield value when the original value is missing or zero, and the area is in hectares.

It checks a similar condition as in step 2 but for acres. If FCY is missing or zero, and the area units are in acres, it sets a default value for FCY based on the formula: 4.5 * area.

It then adjusts the FCY value based on the provided area and area units:

If the area is greater than 1, it divides FCY by the area if the units are in hectares ("ha"). Otherwise, it multiplies FCY by (2.47105 / area) if the units are in acres.
If the area is less than or equal to 1, it multiplies FCY by the area if the units are in hectares ("ha"). Otherwise, it multiplies FCY by (2.47105 * area) if the units are in acres.
Finally, the function returns the preprocessed FCY value, which has been adjusted based on the provided area and area units.
'''
def fcy_preprocessing(FCY, areaUnits, area):
    # Convert FCY to numeric
    # User has given correct FCY
    FCY = float(FCY)

    # Set default value for FCY if zero or NA
    if (FCY is None or FCY == 0) and areaUnits == "ha":
        FCY = 11.25 * area
    elif (FCY is None or FCY == 0) and areaUnits == "acre":
        FCY = 4.5 * area

    # Updated FCY to handle when area is less than 0
    if area > 1:
        FCY = FCY / area if areaUnits == "ha" else FCY * (2.47105 / area)
    else:
        FCY = FCY * area if areaUnits == "ha" else FCY * (2.47105 * area)

    return FCY
