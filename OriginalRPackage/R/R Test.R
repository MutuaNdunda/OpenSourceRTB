STATEREGION <- "Ogun"; LGADistrict <- "Shagamu" ## refers to STATE and LGA for NG and 

cassUP = NA; cassUW = 1000; cassPD = "roots"
cassUP_m1 = NA; cassUP_m2 = NA; cassUP_p1 = NA; cassUP_p2 = NA 
maizeUP = NA; maizePD = "grain"
sweetPotatoPD <- "tubers"; sweetPotatoUW <- 100; sweetPotatoUP = 112500

area = 1; areaUnits = "ha"
cost_manual_ploughing = cost_manual_harrowing = cost_manual_ridging = NA
cost_tractor_ploughing = cost_tractor_harrowing = cost_tractor_ridging = NA
cost_weeding1 = cost_weeding2 = NA
cost_LMO_areaBasis = "areaUnit"
tractor_plough = tractor_harrow = tractor_ridger = TRUE
ploughing = TRUE; ridging = TRUE; harrowing = FALSE
method_ploughing = method_ridging = "manual"; method_harrowing =  "N/A"
PD_window = 1; HD_window = 1; 
saleSF = TRUE; nameSF = "PsaltryMarketers"
fallowType = "bush"; fallowHeight = 100; fallowGreen = TRUE; problemWeeds = TRUE; fallowAge = 0.5
FR = IC = PP = SPH = SPP = WM = TRUE
intercrop = NA

PD = "2019-06-20"; HD = "2020-03-01"
country = "NG";  FCY = 11.25; CMP = 2; riskAtt = 1


### Check this one out ###
areaUnits = 'ha'
cost_LMO_areaBasis = "areaField"
tractor_plough = TRUE
tractor_ridger = TRUE
cost_manual_ploughing = 30000
cost_manual_ridging = 340000
cost_tractor_ploughing = 67000
cost_tractor_ridging = 650000
cost_weeding1 = NA
cost_weeding2 = NA
areaHa = areaHA_function('ha',5)    
country = "NG"


areaUnits = 'ha'
area = 14
FCY = 90
access_tractor = T
cassUP = 45000
cassUW= 1000
ploughing=T
ridging= T
method_ploughing= 'manual'
method_ridging= 'tractor'
cost_manual_ploughing= 45000
cost_manual_ridging=45000
cost_tractor_ploughing=67000
cost_tractor_ridging=89000
cost_weeding1=NA
cost_weeding2=NA
weeding=NA
country= 'NG'
custom_area = NA


cost_lmo_creation <- function(areaUnits, cost_LMO_areaBasis, tractor_plough, tractor_ridger, cost_manual_ploughing, cost_manual_ridging, cost_tractor_ploughing, cost_tractor_ridging, cost_weeding1, cost_weeding2, areaHa, country) {
  
  costLMO <- data.frame(
    operation = c(rep(c("ploughing", "ridging"), 2), "weeding1", "weeding2"),
    method = c(rep("manual", 2), rep("tractor", 2), NA, NA),
    cost = c(
      cost_manual_ploughing, cost_manual_ridging,
      cost_tractor_ploughing, cost_tractor_ridging,
      cost_weeding1, cost_weeding2
    ),
    area = ifelse(
      cost_LMO_areaBasis == "areaField",
      areaHa,
      ifelse(areaUnits == "acre", 0.404686, ifelse(areaUnits == "ha", 1, 0.0001))
    )
  )
  
  costLMO$costHa <- costLMO$cost / costLMO$area
  costLMO <- subset(costLMO, select = -c(area, cost))
  
  # Define default values based on country
  default_values <- switch(
    country,
    "NG" = c(
      manual_ploughing = 17000 * 2.47105,
      manual_ridging = 12000 * 2.47105,
      tractor_ploughing = 6000 * 2.47105,
      tractor_ridging = 6000 * 2.47105,
      weeding1 = 12500 * 2.47105,
      weeding2 = 12500 * 2.47105
    ),
    "TZ" = c(
      manual_ploughing = 432433.9,
      manual_ridging = 555986.2,
      tractor_ploughing = 370657.5,
      tractor_ridging = 284170.9,
      weeding1 = 100000 * 2.47105,
      weeding2 = 850000 * 2.47105
    )
  )
  
  # Update costHa with default values if missing
  costLMO[costLMO$operation == "ploughing" & costLMO$method == "manual" & is.na(cost_manual_ploughing), ]$costHa <- default_values["manual_ploughing"]
  costLMO[costLMO$operation == "ridging" & costLMO$method == "manual" & is.na(cost_manual_ridging), ]$costHa <- default_values["manual_ridging"]
  costLMO[costLMO$operation == "ploughing" & costLMO$method == "tractor" & is.na(cost_tractor_ploughing) & tractor_plough, ]$costHa <- default_values["tractor_ploughing"]
  costLMO[costLMO$operation == "ridging" & costLMO$method == "tractor" & is.na(cost_tractor_ridging) & tractor_ridger, ]$costHa <- default_values["tractor_ridging"]
  costLMO[costLMO$operation == "weeding1" & is.na(cost_weeding1), ]$costHa <- default_values["weeding1"]
  costLMO[costLMO$operation == "weeding2" & is.na(cost_weeding2), ]$costHa <- default_values["weeding2"]
  
  return(costLMO)
}

