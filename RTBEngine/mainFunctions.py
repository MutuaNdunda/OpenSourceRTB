getpprecommendations <- function(areaUnits,area,custom_area,
                                 FCY,access_tractor,
                                 cassUP, cassUW = NA,
                                 ploughing,ridging, method_ploughing,method_ridging,
                                 cost_manual_ploughing,cost_manual_ridging,
                                 cost_tractor_ploughing,cost_tractor_ridging,
                                 cost_weeding1 = NA ,cost_weeding2 = NA,
                                 weeding, gender,
                                 country, lang_id){
  
  #SHORT DEF:   Function to obtain tillage recommendations (step 4 of 6 steps).
  #RETURNS:     dataframe with cost benefit for various combinations of ploughing and ridging.
  #DESCRIPTION: Function to obtain recommendations on ploughing and ridging. Returns a dataframe with all possible combinations of
  #             ploughing (none, manual, tractor) and ridging (none, manual, tractor), ordered by decreasing net returns and increasing
  #             tillage intensity (riding then ploughing)
  #INPUT:       See Cassava Crop Manager function for details
  #Setting default parameters 
  cassPD = "roots"
  cost_LMO_areaBasis = "areaField"

  
  #Conditions for setting up tractor plough and ridger.
  access_tractor = access_tractor
  if(access_tractor == FALSE){
    #Set tractor_plough and ridger to false
    tractor_plough = FALSE
    tractor_ridger = FALSE
    ##Set the method of ploughing and ridging to manual.
    method_ploughing = "manual"
    method_ridging = "manual"
    
  }else{
    #Set tractor_plough and ridger to TRUE
    tractor_plough = TRUE
    tractor_ridger = TRUE
  }
  
  #Stop learner journey if plough and ridging is False 
  if ((ploughing == FALSE) & (ridging == FALSE)) {
    return("We cannot give you a recommendation because you are currently not ploughing or tilling your land")
  }
  #Set method of ploughing to NA, when ploughing == FALSE
  if(ploughing == FALSE){
    method_ploughing = "N/A"
  }
  #Set method of ridging to NA, when ridging == FALSE
  if(ridging == FALSE){
    method_ridging = "N/A"
  }
  
  #Input conversion and transformation
  #Handling area input
  area = area_input_manipulation(area = area, custom_area = custom_area)
  #Stop learner journey if area is not numeric)
  area = as.numeric(area)
  if (is.na(area)) {
    return("No, recommendation invalid area input")
  }
  
  #Setting FCY to default value for FCY
  FCY = fcy_preprocessing(FCY=FCY, areaUnits = areaUnits, area = area)
  cassUP = response_trans(cassUP)
  cassUW = response_trans(cassUW)
  #Cost weeding
  cost_weeding1 = response_trans(cost_weeding1)
  cost_weeding2 = response_trans(cost_weeding2)
  #Cost land operation
  cost_manual_ploughing = response_trans(cost_manual_ploughing)
  cost_manual_ridging = response_trans(cost_manual_ridging)
  cost_tractor_ploughing = response_trans(cost_tractor_ploughing)
  cost_tractor_ridging = response_trans(cost_tractor_ridging)
  
  #getting preprocessed inputs#
  areaHa = areaHA_function(areaUnits,area)
  costLMO = cost_lmo_creation(areaUnits,cost_LMO_areaBasis, tractor_plough = tractor_plough,tractor_ridger = tractor_ridger,cost_manual_ploughing,cost_manual_ridging, cost_tractor_ploughing,cost_tractor_ridging, cost_weeding1, cost_weeding2,areaHa, country)
  all_rootUP = root_conversion(country, cassPD, cassUP,cassUW,cassUP_m1 = NA, cassUP_m2 = NA ,cassUP_p1 = NA, cassUP_p2 = NA)
  
  #get rootUP for PP only
  rootUP = all_rootUP[[1]]
  
  #creating ploughing and ridging scenarios
  ds <- expand.grid(method_ploughing=c("N/A", "manual", "tractor"), method_ridging=c("N/A", "manual", "tractor"))
  ds$ploughing <- ifelse(ds$method_ploughing=="N/A", FALSE, TRUE)
  ds$ridging <- ifelse(ds$method_ridging=="N/A", FALSE, TRUE)
  ds$cost_ploughing <- ifelse(ds$method_ploughing=="N/A", 0,
                              ifelse(ds$method_ploughing=="manual",
                                     costLMO[costLMO$operation=="ploughing" & costLMO$method=="manual", ]$costHa,
                                     costLMO[costLMO$operation=="ploughing" & costLMO$method=="tractor",]$costHa))
  ds$cost_ridging <- ifelse(ds$method_ridging=="N/A", 0,
                            ifelse(ds$method_ridging=="manual",
                                   costLMO[costLMO$operation=="ridging" & costLMO$method=="manual", ]$costHa,
                                   costLMO[costLMO$operation=="ridging" & costLMO$method=="tractor",]$costHa))
  ds <- na.omit(ds)
  #adding cost saving for weeding
  ds$cost_weeding <- ifelse(ds$ridging, -costLMO[costLMO$operation=="weeding1",]$costHa, 0)
  
  #adding expected yields
  yd <- expand.grid(ploughing=c(FALSE, TRUE), ridging=c(TRUE,FALSE), YL=c("low", "high"))
  yd$RY <- c(rep(10, 4), 20, 25, 15, 22)
  yd <- yd[yd$YL==ifelse(FCY<12.5, "low", "high"),]
  ds <- base::merge(ds, yd)
  ds$RP <- ds$RY * areaHa
  
  #calculating total cost, gross and net revenue
  ds$TC <- (ds$cost_ploughing + ds$cost_ridging + ds$cost_weeding) * areaHa
  ds$GR <- ds$RP * rootUP
  ds$NR <- ds$GR - ds$TC
  
  ds <- subset(ds, select=-c(cost_ploughing, cost_ridging, cost_weeding, YL, RY))
  ds <- ds[order(-ds$NR, ds$ridging, ds$ploughing),] #order by decreasing net revenue, increasing ridging and increasing ploughing so that recommendation is first row
  
  #comparing to current practice
  ds$CP  <- ifelse(ds$ploughing==ploughing & ds$method_ploughing==method_ploughing & ds$ridging==ridging & ds$method_ridging==method_ridging, TRUE, FALSE)
  ds$dTC <- ds$TC - ds[ds$CP==TRUE,]$TC
  ds$dRP <- ds$RP - ds[ds$CP==TRUE,]$RP
  ds$dGR <- ds$GR - ds[ds$CP==TRUE,]$GR
  ds$dNR <- ds$NR - ds[ds$CP==TRUE,]$NR
  
  return(getPPText(ds, country, id = lang_id))
}