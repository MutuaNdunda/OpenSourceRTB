import math
import pandas as pd
from preprocessingFunctions import areaHA_function,root_conversion,cost_lmo_creation,fcy_preprocessing,response_trans

#Testing get Planting practice recommendation
areaUnits = 'ha'
area = 14
FCY = 90
access_tractor = True
cassUP = 45000
cassUW= 1000
ploughing=True
ridging= True
method_ploughing= 'manual'
method_ridging= 'tractor'
cost_manual_ploughing= 45000
cost_manual_ridging=45000
cost_tractor_ploughing=67000
cost_tractor_ridging=89000
cost_weeding1=None
cost_weeding2=None
weeding=None
country= 'NG'

def getpprecommendations(areaUnits, area, custom_area, FCY, access_tractor,
                         cassUP, cassUW=None, ploughing=None, ridging=None,
                         method_ploughing=None, method_ridging=None,
                         cost_manual_ploughing=None, cost_manual_ridging=None,
                         cost_tractor_ploughing=None, cost_tractor_ridging=None,
                         cost_weeding1=None, cost_weeding2=None, weeding=None,country=None):

    # Set default parameters
    cassPD = "roots"
    cost_LMO_areaBasis = "areaField"

    # Stop learner journey if plough and ridging is False
    if not (ploughing or ridging):
         print("We cannot give you a recommendation because you are currently not ploughing or tilling your land")

    # Set method of ploughing to NA when ploughing is False
    if not ploughing:
        method_ploughing = "N/A"

    # Set method of ridging to NA when ridging is False
    if not ridging:
        method_ridging = "N/A"

    # Input conversion and transformation
    area = float(area)
    if math.isnan(area):
        print("No, recommendation invalid area input")

    # Setting FCY to default value for FCY
    FCY = fcy_preprocessing(FCY, areaUnits, area)

    cassUP = response_trans(cassUP)
    cassUW = response_trans(cassUW)

    # Cost weeding
    cost_weeding1 = response_trans(cost_weeding1)
    cost_weeding2 = response_trans(cost_weeding2)

    # Cost land operation
    cost_manual_ploughing = response_trans(cost_manual_ploughing)
    cost_manual_ridging = response_trans(cost_manual_ridging)
    cost_tractor_ploughing = response_trans(cost_tractor_ploughing)
    cost_tractor_ridging = response_trans(cost_tractor_ridging)

    # Getting preprocessed inputs
    areaHa = areaHA_function(areaUnits, area)
    costLMO = cost_lmo_creation(areaUnits, cost_LMO_areaBasis, cost_manual_ploughing, cost_manual_ridging,
                                cost_tractor_ploughing, cost_tractor_ridging, cost_weeding1, cost_weeding2, areaHa, country)

    all_rootUP = root_conversion(country, cassPD, cassUP, cassUW)

    # Get rootUP for PP only
    rootUP = all_rootUP[0]

    # Creating ploughing and ridging scenarios
    import itertools

    methods = ["N/A", "manual", "tractor"]
    scenarios = list(itertools.product(methods, repeat=2))
    ds = pd.DataFrame(scenarios, columns=["method_ploughing", "method_ridging"])
    ds["ploughing"] = ds["method_ploughing"].apply(lambda x: False if x == "N/A" else True)
    ds["ridging"] = ds["method_ridging"].apply(lambda x: False if x == "N/A" else True)

    ds["cost_ploughing"] = ds["method_ploughing"].apply(
        lambda x: 0 if x == "N/A" else costLMO[(costLMO["operation"] == "ploughing") &
                                                 (costLMO["method"] == x)]["costHa"].values[0])

    ds["cost_ridging"] = ds["method_ridging"].apply(
        lambda x: 0 if x == "N/A" else costLMO[(costLMO["operation"] == "ridging") &
                                                 (costLMO["method"] == x)]["costHa"].values[0])

    ds.dropna(inplace=True)

    # Adding cost saving for weeding
    ds["cost_weeding"] = ds["ridging"].apply(lambda x: -costLMO[(costLMO["operation"] == "weeding1")]["costHa"].values[0] if x else 0)

    # Adding expected yields
    yd = pd.DataFrame({
        "ploughing": [False, True, False, True],
        "ridging": [True, True, False, False],
        "YL": ["low", "low", "high", "high"],
        "RY": [10, 10, 20, 25]
    })

    yd = yd[yd["YL"] == ("low" if FCY < 12.5 else "high")]

    ds = ds.merge(yd, left_on=["ploughing", "ridging"], right_on=["ploughing", "ridging"])
    ds["RP"] = ds["RY"] * areaHa

    # Calculating total cost, gross and net revenue
    ds["TC"] = (ds["cost_ploughing"] + ds["cost_ridging"] + ds["cost_weeding"]) * areaHa
    ds["GR"] = ds["RP"] * rootUP
    ds["NR"] = ds["GR"] - ds["TC"]

    ds = ds[["ploughing", "method_ploughing", "ridging", "method_ridging", "cost_ploughing", "cost_ridging",
             "cost_weeding", "YL", "RY", "RP", "TC", "GR", "NR"]]

    ds = ds.sort_values(by=["NR", "ridging", "ploughing"], ascending=[False, True, True])

    # Comparing to current practice
    ds["CP"] = (ds["ploughing"] == ploughing) & (ds["method_ploughing"] == method_ploughing) & \
              (ds["ridging"] == ridging) & (ds["method_ridging"] == method_ridging)
    ds["dTC"] = ds["TC"] - ds[ds["CP"]]["TC"].values[0]
    ds["dRP"] = ds["RP"] - ds[ds["CP"]]["RP"].values[0]
    ds["dGR"] = ds["GR"] - ds[ds["CP"]]["GR"].values[0]
    ds["dNR"] = ds["NR"] - ds[ds["CP"]]["NR"].values[0]

    return ds
