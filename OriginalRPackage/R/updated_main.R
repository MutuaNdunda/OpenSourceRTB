getpprecommendations <- function(areaUnits,area,
                                 FCY,access_tractor,
                                 cassUP, cassUW = NA,
                                 ploughing,ridging, method_ploughing,method_ridging,
                                 cost_manual_ploughing,cost_manual_ridging,
                                 cost_tractor_ploughing,cost_tractor_ridging,
                                 cost_weeding1 = NA ,cost_weeding2 = NA,
                                 weeding,
                                 country){
  
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
  
  return(ds)
}

getPPText <- function(ds,country, id){
  library(stringr)
  #We may want to change method_ploughing and method_ridging to character in the getPPrecommendations function?
  ds$method_ploughing <- as.character(ds$method_ploughing)
  ds$method_ridging   <- as.character(ds$method_ridging)
  
  pp_rec_text = read.csv("pp_text.csv")
  
  if(ds[1,]$CP){
    
    firsr_rec  = pp_rec_text[(pp_rec_text$lang_id == id & pp_rec_text$rec_part == 1),6]
    if(ds[1, ]$method_ploughing == "N/A"){
      rec_plough = pp_rec_text[(pp_rec_text$lang_id == id & pp_rec_text$rec_part == 2),6]
    }else if(ds[1, ]$method_ploughing == "manual"){
      rec_plough = pp_rec_text[(pp_rec_text$lang_id == id & pp_rec_text$rec_part == 3),6]
    }else if (ds[1, ]$method_ploughing == "tractor"){
      rec_plough = pp_rec_text[(pp_rec_text$lang_id == id & pp_rec_text$rec_part == 4),6]
    } 
    
    if(ds[1, ]$method_ridging == "N/A"){
      rec_ridge = pp_rec_text[(pp_rec_text$lang_id == id & pp_rec_text$rec_part == 5),6]
      rec_ridge = paste(" ",rec_ridge)
    }else if (ds[1, ]$method_ridging == "manual"){
      rec_ridge = pp_rec_text[(pp_rec_text$lang_id == id & pp_rec_text$rec_part == 6),6]
      rec_ridge = paste(" ",rec_ridge)
    } else if (ds[1, ]$method_ridging == "tractor"){
      rec_ridge = pp_rec_text[(pp_rec_text$lang_id == id & pp_rec_text$rec_part == 7),6]
      rec_ridge = paste(" ",rec_ridge)
    }
    
    last_rec  =  pp_rec_text[(pp_rec_text$lang_id == id & pp_rec_text$rec_part == 8),6]
    rec = paste0(firsr_rec, rec_plough, rec_ridge, last_rec)
    
  }else{
    
    if(ds[1, ]$ploughing & ds[1, ]$ridging)   {recT <- paste(pp_rec_text[(pp_rec_text$lang_id == id & pp_rec_text$rec_part == 9),6], ds[1, ]$method_ploughing,  pp_rec_text[(pp_rec_text$lang_id == id & pp_rec_text$rec_part == 10),6], ds[1, ]$method_ridging, pp_rec_text[(pp_rec_text$lang_id == id & pp_rec_text$rec_part == 11),6])}
    if(ds[1, ]$ploughing & !ds[1, ]$ridging)  {recT <- paste(pp_rec_text[(pp_rec_text$lang_id == id & pp_rec_text$rec_part == 9),6], ds[1, ]$method_ploughing, pp_rec_text[(pp_rec_text$lang_id == id & pp_rec_text$rec_part == 12),6])}
    #Handling third case
    if(!ds[1, ]$ploughing & ds[1, ]$ridging){
      recT_first <- pp_rec_text[(pp_rec_text$lang_id == id & pp_rec_text$rec_part == 13),6]
      
      if (ds[1, ]$method_ridging == "manual"){
        recT_sec = pp_rec_text[(pp_rec_text$lang_id == id & pp_rec_text$rec_part == 14),6]
      }else{
        recT_sec = pp_rec_text[(pp_rec_text$lang_id == id & pp_rec_text$rec_part == 15),6]
      }
      recT_third = pp_rec_text[(pp_rec_text$lang_id == id & pp_rec_text$rec_part == 16),6]
      
      recT = paste(recT_first, recT_sec, recT_third)
      #print(recT)   
    }
    
    if(!ds[1, ]$ploughing & !ds[1, ]$ridging) {recT <- pp_rec_text[(pp_rec_text$lang_id == id & pp_rec_text$rec_part == 17),6]}
    
    currency <- ifelse(country == "NG", "NGN", "TZS")
    changeTC <- ds[1, ]$dTC
    dTC <- formatC(signif(abs(ds[1, ]$dTC), digits=3), format="f", big.mark=",", digits=0)
    dNR <- formatC(signif(ds[1, ]$dNR, digits=3), format="f", big.mark=",", digits=0)
    dRP <- signif(ds[1, ]$dRP, digits=2)
    
    if(dTC == 0){
      recC <- pp_rec_text[(pp_rec_text$lang_id == id & pp_rec_text$rec_part == 18),6]
    }else{
      #change decrease increase for swahili language
      if (id == 2){
        decrease  = "itapunguza"
        increase = "itaongeza"
      } else{
        decrease  = pp_rec_text[(pp_rec_text$lang_id == id & pp_rec_text$rec_part == 24),6]
        increase =  pp_rec_text[(pp_rec_text$lang_id == id & pp_rec_text$rec_part == 25),6]
      }
      recC <- paste(pp_rec_text[(pp_rec_text$lang_id == id & pp_rec_text$rec_part == 19),6], ifelse(changeTC  < 0, decrease, increase), pp_rec_text[(pp_rec_text$lang_id == id & pp_rec_text$rec_part == 20),6], currency, " ", dTC, ". ")
    }
    
    if(dRP == 0 & dNR == 0) {recP <- pp_rec_text[(pp_rec_text$lang_id == id & pp_rec_text$rec_part == 21),6]}
    if(dRP == 0 & dNR > 0)  {recP <- paste(pp_rec_text[(pp_rec_text$lang_id == id & pp_rec_text$rec_part == 22),6], currency, " ", dNR, ".")}
    if(dRP != 0 & dNR == 0) {recP <- paste(pp_rec_text[(pp_rec_text$lang_id == id & pp_rec_text$rec_part == 23),6], ifelse(ds[1, ]$TC < 0, pp_rec_text[(pp_rec_text$lang_id == id & pp_rec_text$rec_part == 24),6], pp_rec_text[(pp_rec_text$lang_id == id & pp_rec_text$rec_part == 25),6]), pp_rec_text[(pp_rec_text$lang_id == id & pp_rec_text$rec_part == 26),6], dRP, pp_rec_text[(pp_rec_text$lang_id == id & pp_rec_text$rec_part == 27),6], ", ",pp_rec_text[(pp_rec_text$lang_id == id & pp_rec_text$rec_part == 29),6],
                                           pp_rec_text[(pp_rec_text$lang_id == id & pp_rec_text$rec_part == 28),6])}
    if(dRP != 0 & dNR != 0) {recP <- paste(pp_rec_text[(pp_rec_text$lang_id == id & pp_rec_text$rec_part == 23),6], ifelse(ds[1, ]$TC < 0, pp_rec_text[(pp_rec_text$lang_id == id & pp_rec_text$rec_part == 24),6], pp_rec_text[(pp_rec_text$lang_id == id & pp_rec_text$rec_part == 25),6]), pp_rec_text[(pp_rec_text$lang_id == id & pp_rec_text$rec_part == 26),6], dRP, pp_rec_text[(pp_rec_text$lang_id == id & pp_rec_text$rec_part == 27),6], ifelse(ds[1, ]$TC < 0, pp_rec_text[(pp_rec_text$lang_id == id & pp_rec_text$rec_part == 29),6], pp_rec_text[(pp_rec_text$lang_id == id & pp_rec_text$rec_part == 30),6]),
                                           pp_rec_text[(pp_rec_text$lang_id == id & pp_rec_text$rec_part == 31),6], currency, " ", dNR)}
    
    rec <- paste0(recT, recC, recP)
  }
  
  #TODO: This only provides the minimal information to return to the user. We may consider adding following information:
  #1. Beware that planting on flat may not be advisable in your specific conditions. You should ridge if the land is sometimes very wet (water-logging problems), if controlling weed is very challenging, if the soil is very clayey, or if you plan to harvest during the dry season.
  #2. We currently do not consider costs and benefits of harrowing - we have not investigated this.
  #3. Explicit reasons underlying recommendations (driven by cost-saving or revenue increase).
  #4. Our selection of the best option may differ from the one by the farmer. A farmer may be willing to choose an option that has a lower net revenue change than the recommended, but also a lower cost.
  #5. Possible issues with the input data - especially if user provides unrealistic prices.
  rec <- str_squish(rec)
  return(rec)
}
