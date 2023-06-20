#Intercropping NG ####
geticrecommendations <- function(areaUnits, area,custom_area, CMP = 1:5, maizePD,custom_maizeUP,maizeUP,maizeUW, riskAtt, urea_price = NA, npk_price = NA, dap_price = NA,country = "NG", lang_id, gender){

  #SHORT DEF:   Function to obtain recommendations on cassava-maize intercropping.
  #RETURNS:     list of 2 dataframes: (i) cost benefit analysis for most profitable system, and (ii) fertilizer rates to apply.
  #             Returns (i) a 1-row datafraLme cost-benefit parameters (extra yield, cost and net revenue, and whether to apply
  #             fertilizer and to plant maize at high density, and why (not)) , and (ii) a data.frame with types of fertilizer and rates to apply (zeros included).
  #INPUT:       See Cassava Crop Manager function for details
   
  #load required required library
  library("limSolve")
  
  #Pull language templates
  #setting up language id
  id = lang_id
  #pull recommendation template for recommendation.
  ic_rec_text = rec_text_all[rec_text_all$rec_id == 4, ]
  
  #Input conversion and transformation
  area = area_input_manipulation(area = area, custom_area = custom_area)
  if(is.na(maizeUP)){
    maizeUP = custom_maizeUP
  }
  
  CMP = as.numeric(CMP)
  ristAtt = as.numeric(riskAtt)

  #areaHa computation 
  areaHa = areaHA_function(areaUnits, area)

  #cobUP computation
  maizePD = tolower(maizePD)
  maizeUP = response_trans(maizeUP)
  maizeUW = response_trans(maizeUW)
  cobUP = cobUp_function(maizePD, maizeUP, maizeUW)
  
  #Getting user Fertilizer prices 
  fertilizers = fertilizer_input(urea_price, npk_price, dap_price, country)

  #calculating expected yield increase from fertilizer
  #calculating expected yield increase from fertilizer
  maizeY <- data.frame(CMP=1:5, 
                       dY=c(0, 6500, 4000, 2500, 0))
  dMY <- maizeY[maizeY$CMP == CMP,]$dY
  dMP <- dMY * areaHa #extra maize production for the area of the field
  
  #extra gross revenue from fertilizer
  dGR <- dMP * cobUP
  
  if(dGR==0){
    reason_F <- ifelse(CMP==1, ic_rec_text[(ic_rec_text$lang_id == id & ic_rec_text$rec_part== 1),6], ic_rec_text[(ic_rec_text$lang_id == id & ic_rec_text$rec_part== 2),6])
    dNR <- dNRmin <- 0
  }else{
    #calculating fertilizer requirement
    E <- t(data.matrix(fertilizers[,2:4]))
    # F <- c(91, 19.5, 37.5) #ideally 2 bags of urea + 6 bags of NPK15:15:15
    F <- c(91, 21, 37.5) #ideally 2 bags of urea + 6 bags of NPK15:15:15
    G <- diag(nrow(fertilizers))
    H <- rep(0, nrow(fertilizers))
    Cost <- fertilizers$price
    
    #calculating fertilizer recommendation and total cost of fertilizer
    FR <- linp(E, F, G, H, Cost)$X
    FR[FR<25] <- 0 #dropping all rates less than 25 kg/ha
    FR <- FR * areaHa #adjusting to field area
    
    #calculating total cost
    dTC <- c(FR %*% fertilizers$price) 
    
    #evaluating if a solution was found
    if(dTC==0){
      dGR <- 0 
      dMP <- 0
      reason_F <- ic_rec_text[(ic_rec_text$lang_id == id & ic_rec_text$rec_part== 3),6] 
    }else{
      reason_F <- "appropriate fertilizer is available"
    }
    #net revenue increase from fertilizer
    dNR <- dGR - dTC
    
    #minimal required net revenue increase from fertilizer needed (taking into account risk attitude of user)
    dNRmin <- dTC * ifelse(riskAtt == 0, 1.8, ifelse(riskAtt == 1, 1, 0.2))
    
  }
  
  
  #check profitability of fertilizer use
  if(dNR > dNRmin){
    rec_F <- TRUE
    reason_F <- "fertilizer use is sufficiently profitable"
  }else{
    dMP <- 0
    dTC <- 0
    dGR <- 0
    dNR <- 0
    FR <- 0 # FR <- FR * 0
    rec_F <- FALSE
    reason_F <- ic_rec_text[(ic_rec_text$lang_id == id & ic_rec_text$rec_part== 4),6]
  }
  
  #recommendation on high density maize planting
  rec_D <- ifelse(rec_F == TRUE | CMP == 5, TRUE, FALSE)
  reason_D <- ifelse(rec_F == TRUE,  ic_rec_text[(ic_rec_text$lang_id == id & ic_rec_text$rec_part== 5),6], ifelse(CMP==5,  ic_rec_text[(ic_rec_text$lang_id == id & ic_rec_text$rec_part== 2),6], NA))
  
  #output
  if(!is.na(reason_D)){
    rec <- data.frame(dMP=dMP, #extra maize production expected (in nr of cobs)
                      dNR=dNR, #net revenue increase from fertilizer use (in local currency)
                      dTC=dTC, #extra cost for fertilizer use (in local currency)
                      rec_F=rec_F, #TRUE or FALSE indicating if fertilizer application is recommended 
                      rec_D=rec_D, #TRUE or FALSE indicating if high density maize planting is recommended
                      reason_F=reason_F, #reason why fertilizer application is not recommended
                      reason_D=reason_D  #reason why high maize density is recommended
    )
    
  }else{
    rec <- data.frame(dMP=dMP, #extra maize production expected (in nr of cobs)
                      dNR=dNR, #net revenue increase from fertilizer use (in local currency)
                      dTC=dTC, #extra cost for fertilizer use (in local currency)
                      rec_F=rec_F, #TRUE or FALSE indicating if fertilizer application is recommended 
                      rec_D=rec_D, #TRUE or FALSE indicating if high density maize planting is recommended
                      reason_F=reason_F #reason why fertilizer application is not recommended
    )
    
  }
  
  fertilizer_rates <- data.frame(type=fertilizers$type, rate=FR) #fertilizer rates to apply
  fertilizer_rates <- fertilizer_rates[fertilizer_rates$rate > 0,]
  
  #Create data frame
  ds = list(rec=rec, fertilizer_rates=fertilizer_rates)

  #get recommendation text
  ic_text = getICrecText(ds, country, id = lang_id)
  return(ic_text)
}

#Intercropping TZ #####
geticrecommendationstz <- function(areaUnits,area,custom_area,
                                  FCY,
                                  cassUP = NA, cassUW = NA, sweetPotatoUP = NA,sweetPotatoUW = NA,
                                  urea_price, npk_price, dap_price,
                                  country = "TZ", gender,
                                  riskAtt, 
                                  lang_id) {
  #SHORT DEF:   Function to obtain recommendations on cassava-sweet potato intercropping.
  #RETURNS:     list of 2 dataframes: (i) cost benefit analysis for most profitable system, and (ii) fertilizer rates to apply.
  #DESCRIPTION: Function to obtain recommendations on cassava-sweet potato intercropping.
  #             Returns (i) a 1-row dataframe cost-benefit parameters (extra yield, cost and net revenue, and whether to apply
  #             fertilizer and whether to intercrop, and why (not)) , and (ii) a data.frame with types of fertilizer and rates to apply (zeros included).
  #INPUT:       See Cassava Crop Manager function for details
  
  #load required required library
  library("limSolve")
  #Set defaults
  cassPD = "roots"
  sweetPotatoPD = "tubers" 
  
  #Input conversion and transformation.
  area = area_input_manipulation(area = area, custom_area = custom_area)
  #Stop learner journey if area is not numeric)
  area = as.numeric(area)
  if (is.na(area)) {
    return("No, recommendation invalid area input")
  }
  
  #Other conversion
  FCY = fcy_preprocessing(FCY=FCY, areaUnits = areaUnits, area = area)
  cassUP = response_trans(cassUP)
  cassUW = response_trans(cassUW)
  sweetPotatoUP = response_trans(sweetPotatoUP)
  sweetPotatoUW = response_trans(sweetPotatoUW)
  ristAtt = as.numeric(riskAtt)
  
  # areaHa computation
  areaHa = areaHA_function(areaUnits, area)
  #Getting user Fertilizer prices
  fertilizers = fertilizer_input(urea_price, npk_price, dap_price, country)
  
  #Computation of all rootup
  all_rootUP = root_conversion(country, cassPD, cassUP,cassUW,cassUP_m1 = NA, cassUP_m2 = NA ,cassUP_p1 = NA, cassUP_p2 = NA)
  #get rootUP for PP only
  rootUP = all_rootUP[[1]]
  
  #computation of all tuberUP
  tuberUP = tuber_conversion(country,sweetPotatoPD, sweetPotatoUP,sweetPotatoUW)
  
  #specify id
  id = lang_id
  
  #Pull recommendation templates
  ic_rec_text = rec_text_all[rec_text_all$rec_id == 5, ]
  
  #calculating expected yield increase from fertilizer
  FSY <- 0.7 * FCY #expected yield of a sweet potato monocrop
  GR_MC <- FCY * rootUP #gross revenue of cassava monocrop
  GR_IC <- GR_MC * 0.8 + 0.6 * FSY * tuberUP
  rec_IC <- GR_MC < GR_IC
  
  if (rec_IC) {
    #calculating fertilizer requirement
    E <- t(data.matrix(fertilizers[, 2:4]))
    F <-
      c(68, 19.6, 56.8) #ideally 2 bags of urea + 6 bags of NPK15:15:15
    G <- diag(nrow(fertilizers))
    H <- rep(0, nrow(fertilizers))
    Cost <- fertilizers$price
    
    #calculating fertilizer recommendation and total cost of fertilizer
    FR <- linp(E, F, G, H, Cost)$X
    FR[FR < 25] <- 0 #dropping all rates less than 25 kg/ha
    FR <- FR * areaHa #adjusting to field area
    
    #calculating total cost
    dTC <- c(FR %*% fertilizers$price)
    dGR <- ifelse(FCY > 30, 0, ifelse(FCY > 20, FCY * 0.8 * 0.2 * rootUP + FSY * 0.6 * 0.1 * tuberUP, FCY * 0.8 * 0.4 * rootUP + FSY * 0.6 * 0.2 * tuberUP)) #gross revenue increase: 40% yield increase in cassava + 20% yield increase in sweet potato, but not in fields with yields above 20 t/ha
    
    #evaluating if a solution was found
    if (dTC == 0) {
      dGR <- 0
      reason_F <- ic_rec_text[(ic_rec_text$lang_id == id & ic_rec_text$rec_part == 5), 6]
      rec_F <- FALSE
    } else{
      reason_F <- ic_rec_text[(ic_rec_text$lang_id == id &  ic_rec_text$rec_part == 6), 6]
      rec_F <- TRUE
    }
  } else{
    dTC <- 0
    FR <- 0
    dGR <- 0
    reason_F <- ic_rec_text[(ic_rec_text$lang_id == id & ic_rec_text$rec_part== 7),6]
    rec_F <- FALSE
  }
  
  #net revenue increase from fertilizer
  dNR <- dGR - dTC
  
  if (dTC > 0) {
    #minimal required net revenue increase from fertilizer needed (taking into account risk attitude of user)
    dNRmin <-
      dTC * ifelse(riskAtt == 0, 1.8, ifelse(riskAtt == 1, 1, 0.2))
    
    #check profitability of fertilizer use
    if (dNR > dNRmin) {
      rec_F <- TRUE
      reason_F <- ic_rec_text[(ic_rec_text$lang_id == id & ic_rec_text$rec_part== 6),6]
    } else{
      dTC <- 0
      dGR <- 0
      dNR <- 0
      FR <- FR * 0
      rec_F <- FALSE
      reason_F <- ic_rec_text[(ic_rec_text$lang_id == id & ic_rec_text$rec_part== 6),6]
    }
  }
  
  #output
  rec <-
    data.frame(
      rec_IC = rec_IC,
      #boolean indicating whether intercropping is recommended (more profitable than monocropping cassava)
      rec_F = rec_F,
      #TRUE or FALSE indicating if fertilizer application is recommended
      dNR = dNR,
      #net revenue increase from fertilizer use (in local currency)
      dTC = dTC,
      #extra cost for fertilizer use (in local currency)
      reason_F = reason_F #reason why fertilizer application is not recommended
    )
  
  fertilizer_rates <-
    data.frame(type = fertilizers$type, rate = FR) #fertilizer rates to apply
  fertilizer_rates <- fertilizer_rates[fertilizer_rates$rate > 0, ]
  
  ds = list(rec = rec,
            fertilizer_rates = fertilizer_rates)
  
  ic_text = getICrecText(ds, country, id = lang_id)
  return(ic_text)
}

#PP #####
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

#FR ######
getfertrecom <- function(areaUnits,
                         area,
                         custom_area,
                         state,
                         LGA,
                         plm,
                         FCY,
                         cassUP = NA,
                         cassUW,
                         urea_price = NA,
                         npk_price = NA,
                         dap_price = NA,
                         country,
                         gender,
                         lang_id) {
  
  #Input conversion and transformation
  #Handling area input
  area = area_input_manipulation(area = area, custom_area = custom_area)
  #Stop learner journey if area is not numeric)
  area = as.numeric(area)
  if (is.na(area)) {
    return("No, recommendation invalid area input")
  }
  
  #Setting cassPD to roots
  cassPD = "roots"
  plm = as.numeric(plm)
  #convert put the default value for FCY
  FCY = fcy_preprocessing(FCY=FCY, areaUnits = areaUnits, area = area)
  FCY <- min(5, ceiling(FCY /7.5))
  #conversion of cassUp and cassUW
  cassUP = response_trans(cassUP)
  cassUW = response_trans(cassUW)
  #conversion of State and LGA names to lower case
  State = tolower(state)
  LGA = tolower(LGA)
  #Getting preprocessed data
  areaHa = areaHA_function(areaUnits, area)
  ##Compute all the rootup function
  all_rootUP = root_conversion(
    country,
    cassPD,
    cassUP,
    cassUW,
    cassUP_m1 = NA,
    cassUP_m2 = NA ,
    cassUP_p1 = NA,
    cassUP_p2 = NA
  )
  
  #get rootUP for PP only
  rootUP = all_rootUP[[1]]
  #Getting user Fertilizer prices
  fertilizers = fertilizer_input(urea_price, npk_price, dap_price, country)
  ##
  id = lang_id
  
  #Get location precomputed values
  if (country == "NG") {
    flp <- FR_ha_NG_2020
    flp <- flp[flp$HASC1 == State & flp$LGA == LGA,]
  } else if (country == "TZ") {
    flp <- FR_ha_TZ_2020
    flp <-flp[flp$HASC1 == State & flp$LGA == LGA,]
  } else{
    flp <- NULL
  }
  
  #Working on the adjsutment
  if(country== "NG"){
    ### adjusting the fertilizer recommendation for areaHa as defined by ha vs. acre
    flp$rateUrea <- flp$rateUrea * areaHa
    flp$rateNPK151515 <- flp$rateNPK151515 * areaHa
    flp$DY <- flp$DY * areaHa
  }else{
    ### adjusting the fertilizer recommendation for areaHa as defined by ha vs. acre
    flp$rateUrea <- flp$rateUrea * areaHa
    flp$rateNPK171717 <- flp$rateNPK171717 * areaHa
    flp$rateDAP <- flp$rateDAP * areaHa
    flp$DY <- flp$DY * areaHa
  }
  
  #Recommendation engine logic
  if(dim(flp)[1] == 0){
    #Return error if location not available
    fr_rec_text = rec_text_all[rec_text_all$rec_id == 1,]
    return(fr_rec_text[(fr_rec_text$lang_id == id & fr_rec_text$rec_part== 1),6])
  } else{
    res <- NULL
    #subset the flp file by planting month and FCY
    res <- flp[flp$month == plm & flp$FCY == FCY, ]
    if (country == "NG") {
      res$priceUrea <- fertilizers[fertilizers$type == 'urea', "price"]
      res$priceNPK151515 <- fertilizers[fertilizers$type == 'NPK', "price"]
      res$TC <-(res$rateUrea * res$priceUrea) + (res$rateNPK151515 * res$priceNPK151515)
      res$cropSale <- res$DY * rootUP
      res$NR <- res$cropSale - res$TC
    } else{
      res$priceUrea <- fertilizers[fertilizers$type == 'urea', "price"]
      res$priceNPK171717 <-fertilizers[fertilizers$type == 'NPK', "price"]
      res$priceDAP <-fertilizers[fertilizers$type == 'DAP', "price"]
      res$TC <-(res$rateUrea * res$priceUrea) + (res$rateNPK171717 * res$priceNPK171717) + (res$rateDAP * res$priceDAP)
      res$cropSale <- res$DY * rootUP
      res$NR <- res$cropSale - res$TC
    }
    
    
    if (dim(res)[1] == 0) {
      #Return error if location not available
      fr_rec_text = rec_text_all[rec_text_all$rec_id == 1,]
      return(fr_rec_text[(fr_rec_text$lang_id == id & fr_rec_text$rec_part== 1),6])
    } else{
      fertilizer_rates = data.frame(type = gsub("rate", "", names(res)[grepl("rate", names(res))]),
                                    rate = as.numeric(res[, grepl("rate", names(res))]))
      ds <- list(rec = res, fertilizer_rates = fertilizer_rates)
    }
  }
  #Return recommendation text
  text = getFRrecText(ds, country, id = lang_id)
  return(text)
}

# SP ####
getSPrecommendations <- function(areaUnits, area,custom_area,state,
                                 country,lang_id,
                                 LGA,
                                 PD,
                                 HD,
                                 plm,FCY,
                                 PD_window = 1,
                                 HD_window = 1,
                                 saleSF = c(TRUE, FALSE),
                                 nameSF,
                                 cassPD,
                                 cassUP = NA, cassUW = 1000,
                                 cassUP_m1 = NA, cassUP_m2 = NA, cassUP_p1 = NA, cassUP_p2 = NA, gender){
  
  #Set custom values
  cassPD = "roots"
  #Input conversion and transformation
  area = area_input_manipulation(area = area, custom_area = custom_area)
  #Convert PD window
  PD_window = as.numeric(PD_window)
  HD_window = as.numeric(HD_window)
  #If the value of PD is NUll
  #set PD = PLM
  if (is.na(PD)== TRUE) {
    PD = plm
  }
  
  #Assign language ID
  id = lang_id
  #change planting date and 6Harvest date to dates
  PD <- as.Date(PD, format = "%Y-%m-%d")
  # ##Make sure user haverst date is not before planting date.
  check_HD <- as.Date(HD, format = "%Y-%m-%d")
  # #Ensure HD is set well. Taken to Next Year if month is less than planting month
  HD = correct_HD_input(PD,check_HD)
  # #Ensure there is 8 to 15 months time period before harversting
  cassava_mandatory_maturity(PD,HD)
  #FCY 
  FCY = fcy_preprocessing(FCY=FCY, areaUnits = areaUnits, area = area)
  FCY <- min(5, ceiling(FCY /7.5))
  #convert LGA to lower
  LGA = tolower(LGA)

  #convert put the default value for FCY
  cassUP = response_trans(cassUP)
  cassUW = response_trans(cassUW)
  cassUP_m1 = response_trans(cassUP_m1)
  cassUP_m2 = response_trans(cassUP_m2)
  cassUP_p1 = response_trans(cassUP_p1)
  cassUP_p2 = response_trans(cassUP_p2)

  #Get all sf companies##
  all_sf_companies  = unique(tolower(starchPrices$starchFactory))
  
  nameSF = tolower(nameSF)

  if (nameSF %in% all_sf_companies){
    nameSF = nameSF
  }else{
    nameSF = NA
  }

  #read in sp file input#
  if(country == "NG"){
    #res file when country is Nigeria
    res <- WLY_CY_NG
    res$LGA <- tolower(res$LGA)
    yld <- res[res$LGA == LGA, ]
  }else if (country == "TZ"){
    #res file when the country is Tanzania
    res <- WLY_CY_TZ
    res$LGA <- tolower(res$LGA)
    yld <- res[res$LGA == LGA,]
  }else{
    cat("\n")
    #if any other response is gotten
    return("Wrong input for the country variable. Should be 'TZ' or 'NG'")
    cat("\n")
  }

  #TODO: censor recommended planting and harvest dates so they cannot fall in the past.
  #TODO: set threshold for minimal increase in revenue to recommend change in planting and harvest date.

  #Preprocesed input
  areaHa = areaHA_function(areaUnits, area)
  all_rootUP = root_conversion(country, cassPD, cassUP,cassUW,cassUP_m1, cassUP_m2 ,cassUP_p1, cassUP_p2)
  #get rootUP for PP only##
  rootUP = all_rootUP[[1]]
  rootUP_m1 = all_rootUP[[2]]
  rootUP_m2 = all_rootUP[[3]]
  rootUP_p1 = all_rootUP[[4]]
  rootUP_p2 = all_rootUP[[5]]


  if(is.null(yld)){
    #Pull all SP recommendation messages
    sp_rec_text = rec_text_all[rec_text_all$rec_id == 3, ]
    #Return no recommendation for the farm location
    return(sp_rec_text[(sp_rec_text$lang_id == id & sp_rec_text$rec_part== 1),6])
    ds <- NULL
  }else{
    #constituting df with yields within relevant planting and harvest windows
    ds <- expand.grid(rPWnr = seq((-4*PD_window), (4*PD_window), by=2),
                      rHWnr = seq((-4*HD_window), (4*HD_window), by=2))
    ds$PD <- PD + ds$rPWnr*7
    ds$HD <- HD + ds$rHWnr*7
    ds$plw <- as.numeric(format(ds$PD, format = "%W"))
    ds$haw <- round(as.numeric(ds$HD - ds$PD)/7)
    ds <- base::merge(ds, unique(yld), by=c("plw", "haw"))
    
    #converting dry yields to fresh root yields
    ds$RFCY <- getRFY(HD = ds$HD, RDY = ds$CY, country = country)
    ds$RFWY <- getRFY(HD = ds$HD, RDY = ds$WY, country = country)
    
    #scaling predicted yield based on farmer-reported current yield relative to modelled CY and WY:
    ds$RY <- (ds$RFWY - ds$RFCY) / (13.5 - 1.5) / 2.5 * (FCY - 1.5 * 2.5) + ds$RFCY
    ds$RP <- ds$RY * areaHa
    
    #If selling to a starch factory: rootUP is determined by starch content of roots
    if(saleSF){
      
      ds$SC <- 0.75 * ds$WY / ds$RFWY * 100
      #SF <- read.csv("E:/03-projects/ACAI/ODK briefcase storage/created forms/DSTs/media_SPHS/starchPrices.csv")
      SF <- starchPrices
      SF$starchFactory = tolower(SF$starchFactory)
      SF <- SF[SF$starchFactory == nameSF,]
      price <- NULL
      for(i in 1:nrow(ds)){
        price <- c(price, max(SF[SF$minStarch<ds[i,]$SC,]$price))
      }
      ds$rootUP <- price
      ds <- subset(ds, select=-SC)
      #If not selling to a starch factory, user needs to specify rootUP across the harvest window  
    }else{
      dp <- data.frame(rHWnr = c(-8, -4, 0, 4, 8),
                       rootUP = c(rootUP_m2, rootUP_m1, rootUP, rootUP_p1, rootUP_p2))
      #Replace all NA'S with 0
      dp[is.na(dp)] <- 0
      dpm <- suppressWarnings(loess(rootUP ~ rHWnr, data=dp))
      dpp <- data.frame(rHWnr = seq((-4*HD_window), (4*HD_window), by=2), 
                        rootUP = predict(dpm, data.frame(rHWnr=seq((-4*HD_window), (4*HD_window), by=2))))
      ds <- merge(ds, dpp)  
    }
    
    ds$GR <- ds$RP * ds$rootUP 
    ds$CP <- ifelse(ds$rPWnr==0 & ds$rHWnr==0, TRUE, FALSE)
    ds$dGR <- ds$GR - ds[ds$CP==TRUE,]$GR
    
    #sort by decreasing GR, increasing HWnr, decreasing PWnr
    #recommendation is highest GR, earliest harvesting, latest planting combination
    ds <- ds[order(-ds$dGR, ds$rHWnr, -ds$rPWnr),] 
    
  }

  sp_text = getSPrecText(ds, country, id = lang_id)
  return(sp_text)

}

