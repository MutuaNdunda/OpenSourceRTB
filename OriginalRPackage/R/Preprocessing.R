#Conversion of areaUnits into hectares####
areaHA_function <- function(areaUnits, area){
  areaHa <- area / ifelse(areaUnits=="ha", 1, ifelse(areaUnits=="acre", 2.47105, 10000))
  return(areaHa)
}

#Creation of cobUP based on maizeUP, maizeUW #########
cobUp_function <- function(maizePD, maizeUP, maizeUW){
  
  if(maizePD == "fresh_cob"){
    maizeUW <- 1
  }
  
  if(is.na(maizeUP) & maizePD == "fresh_cob") maizeUP <- 50 #default price for 1+ large fresh cob
  if(is.na(maizeUP) & maizePD == "grain")
    {
    maizeUP <- 230 #default price for 1 kg of maize grain
    maizeUW <- 1
  }
#
  cobUP <- ifelse(maizePD == "fresh_cob", maizeUP, maizeUP / maizeUW / 7.64)
  return(cobUP)
}

#Computations of rootUP all ##########
root_conversion <- function(country, cassPD = "roots", cassUP = NA,cassUW = NA,cassUP_m1 = NA, cassUP_m2 = NA ,cassUP_p1 = NA, cassUP_p2 = NA){
  
  rootConv <- data.frame(cassPD = c("roots", "chips", "flour", "gari"),
                         conversion = c(1, 3, 3.2, 3.5))
  
  #Handling when root is 0 or NA . Set cassUW = 1000
  if(cassUW == 0 |is.na(cassUW)){ 
    cassUW = 1000
  }
  
  if(is.na(cassUP) & cassPD=="roots" & country=="NG"){cassUP = 12000; cassUW = 1000}
  if(is.na(cassUP) & cassPD=="chips" & country=="NG"){cassUP = 36000; cassUW = 1000}
  if(is.na(cassUP) & cassPD=="flour" & country=="NG"){cassUP = 38400; cassUW = 1000}
  if(is.na(cassUP) & cassPD=="gari"  & country=="NG"){cassUP = 42000; cassUW = 1000}
  if (is.na(cassUP) & cassPD == "roots" & country == "TZ") {cassUP = 180000;cassUW = 1000}
  if (is.na(cassUP) & cassPD == "chips" & country == "TZ") {cassUP = 540000;cassUW = 1000}
  if (is.na(cassUP) & cassPD == "flour" & country == "TZ") {cassUP = 576000;cassUW = 1000}
  if (is.na(cassUP) & cassPD == "gari"  & country == "TZ") {cassUP = 630000;cassUW = 1000}
  
  rootUP <- cassUP / cassUW / rootConv[rootConv$cassPD==cassPD,]$conversion * 1000

  rootUP_m1 <- cassUP_m1 / cassUW / rootConv[rootConv$cassPD==cassPD,]$conversion * 1000
  rootUP_m2 <- cassUP_m2 / cassUW / rootConv[rootConv$cassPD==cassPD,]$conversion * 1000
  rootUP_p1 <- cassUP_p1 / cassUW / rootConv[rootConv$cassPD==cassPD,]$conversion * 1000
  rootUP_p2 <- cassUP_p2 / cassUW / rootConv[rootConv$cassPD==cassPD,]$conversion * 1000

  rootUP_All = list(rootUP, rootUP_m1, rootUP_m2, rootUP_p1, rootUP_p2)
  #return all rootUp_All
  return(rootUP_All)
}

#Computation of tuberUP ####
tuber_conversion <- function(country,sweetPotatoPD, sweetPotatoUP,sweetPotatoUW){
  #create tuberConv dataframe
  tuberConv <-
    data.frame(sweetPotatoPD = c("tubers", "flour"),
               conversion = c(1, 3.2))
  #conditions to populate default values when NA is provided.
  if (is.na(sweetPotatoUP) & sweetPotatoPD == "tubers" & country == "TZ") {sweetPotatoUP = 280000; sweetPotatoUW = 1000 }
  if (is.na(sweetPotatoUP) & sweetPotatoPD == "flour" & country == "TZ") { sweetPotatoUP = 384000 ; sweetPotatoUW = 1000 }
  #calculation of tuberUP function
  tuberUP <- sweetPotatoUP / sweetPotatoUW / tuberConv[tuberConv$sweetPotatoPD == sweetPotatoPD,]$conversion * 1000
  return(tuberUP)
}

#creation of costlmo dataframe ######
cost_lmo_creation <- function(areaUnits, cost_LMO_areaBasis, tractor_plough,tractor_ridger,cost_manual_ploughing,cost_manual_ridging, cost_tractor_ploughing,cost_tractor_ridging, cost_weeding1, cost_weeding2,areaHa, country){
  
  costLMO <- data.frame(operation = c(rep(c("ploughing", "ridging"),2), "weeding1", "weeding2"),
                        method = c(rep("manual", 2), rep("tractor", 2), NA, NA),
                        cost = c(cost_manual_ploughing, cost_manual_ridging, cost_tractor_ploughing, cost_tractor_ridging, cost_weeding1, cost_weeding2),
                        area = ifelse(cost_LMO_areaBasis=="areaField", areaHa, ifelse(areaUnits=="acre", 0.404686, ifelse(areaUnits=="ha", 1, 0.0001))))
  costLMO$costHa <- costLMO$cost / costLMO$area
  costLMO <- subset(costLMO, select=-c(area, cost))

  # add default values for LMO operations if missing
  if(is.na(cost_manual_ploughing) & country == "NG")                   costLMO[costLMO$operation=="ploughing" & costLMO$method=="manual" ,]$costHa <- 17000 * 2.47105
  if(is.na(cost_manual_ridging) & country == "NG")                     costLMO[costLMO$operation=="ridging"   & costLMO$method=="manual" ,]$costHa <- 12000 * 2.47105
  if(is.na(cost_tractor_ploughing) & tractor_plough & country == "NG") costLMO[costLMO$operation=="ploughing" & costLMO$method=="tractor",]$costHa <- 6000 * 2.47105
  if(is.na(cost_tractor_ridging)   & tractor_ridger & country == "NG") costLMO[costLMO$operation=="ridging"   & costLMO$method=="tractor",]$costHa <- 6000 * 2.47105
  if(is.na(cost_weeding1) & country == "NG")                           costLMO[costLMO$operation=="weeding1", ]$costHa                             <- 12500 * 2.47105
  if(is.na(cost_weeding2) & country == "NG")                           costLMO[costLMO$operation=="weeding2", ]$costHa                             <- 12500 * 2.47105
  
  if(is.na(cost_manual_ploughing) & country == "TZ")                   costLMO[costLMO$operation=="ploughing" & costLMO$method=="manual" ,]$costHa <- 432433.9
  if(is.na(cost_manual_ridging) & country == "TZ")                     costLMO[costLMO$operation=="ridging"   & costLMO$method=="manual" ,]$costHa <- 555986.2
  if(is.na(cost_tractor_ploughing) & tractor_plough & country == "TZ") costLMO[costLMO$operation=="ploughing" & costLMO$method=="tractor",]$costHa <- 370657.5
  if(is.na(cost_tractor_ridging)   & tractor_ridger & country == "TZ") costLMO[costLMO$operation=="ridging"   & costLMO$method=="tractor",]$costHa <- 284170.9
  if(is.na(cost_weeding1) & country == "TZ")                           costLMO[costLMO$operation=="weeding1", ]$costHa                             <- 100000 * 2.47105
  if(is.na(cost_weeding2) & country == "TZ")                           costLMO[costLMO$operation=="weeding2", ]$costHa                             <- 850000 * 2.47105
  return(costLMO)
}

##creation of fertilizer dataframe for Tanzania and Nigeria ######
fertilizer_input <-
  function(urea_price, npk_price, dap_price, country) {
    #check if country is TZ or NG
    if((country %in% c("TZ", "NG") == FALSE)){
      return("Country must be TZ or NG")
      stop()
    }
    
    #if the country == Nigeria##
    if (country == "NG") {
      #convert the inputs into numeric
      price = as.numeric(c(urea_price, npk_price))
      #Get price per KG
      price = price/50
    
      #Add default prices for urea.
      if ((is.na(price[1]) == TRUE | price[1] == 0)) {
        price[1] = 150
      }
      #Add default prices for npk fertilizer.
      if ((is.na(price[2]) == TRUE | price[2] == 0)) {
        price[2] = 170
      }
      
      #Creating the fertilizer data Frame
      fertilizers = data.frame(
        type = c("urea", "NPK"),
        N_cont = c(0.46, 0.15),
        P_cont = c(0, 0.07),
        K_cont = c(0, 0.125)
      )
      
      #Cbind the column to the data frame
      fertilizers = cbind(fertilizers, price)
      return(fertilizers)
      
    } else if (country == "TZ") {
      #convert the inputs into numeric
      price = as.numeric(c(urea_price, dap_price, npk_price))
      #Get price per KG
      price = price/50
      
      #Add default prices for urea.
      if ((is.na(price[1]) == TRUE | price[1] == 0)) {
        price[1] = 1160
      }
      
      #Add default prices for urea.
      if ((is.na(price[2]) == TRUE | price[1] == 0)) {
        price[2] = 1260
      }
      
      #Add default prices for npk fertilizer.
      if ((is.na(price[3]) == TRUE | price[2] == 0)) {
        price[3] = 1200
      }
      
      
      #Creating the fertilizer data Frame
      fertilizers = data.frame(
        type = c("urea", "DAP", "NPK"),
        N_cont = c(0.46, 0.18, 0.17),
        P_cont = c(0, 0.46, 0.083),
        K_cont = c(0, 0, 0.15)
      )
      
      #Cbind the column to the data frame
      fertilizers = cbind(fertilizers, price)
      
      return(fertilizers)
    } else{
      cat("\n")
      #if any other response is gotten
      cat("Kindly check the input for country")
      cat("\n")
    }
    
  }


#dry matter to fresh root yield ####
getRFY <- function(HD,
                   RDY,
                   country = "NG"){

  #SHORT DEF:   Function to convert root DM yield into root fresh matter yield (RFY)
  #RETURNS:     RFY: root fresh yield in the same units as root DM yield input
  #DESCRIPTION: Function to predict root FM yield based on date of harvest and country, using data from gravimetric starch measurements conducted across ACAI trials.
  #INPUT:       HD: harvest date (Date format)
  #             RDY: root dry matter yield (user's units)
  #             country = c("NG", "TZ")

  d  <- as.numeric(strftime(HD, format = "%j"))
  fd <- fd #data.frame with day of the year (dayNr = [1..366]) and %DM (DMCont = [0..100], by country)
  DC <- merge(data.frame(dayNr=d), fd[fd$country==country,], sort=FALSE)$DMCont
  RFY <- RDY / DC * 100

  return(RFY)
}

#Setting FCY to default
#The default value for FCY is 11, when the user responds with something
#differrent, I will convert, set it to 11 or else return the value provided
#preprocessing FCY
# FCY preprocessing ####
fcy_preprocessing <- function(FCY, areaUnits, area) {
  #convert FCY to numeric
  #User has given correct FCY
  FCY = as.numeric(FCY)
  
  #set default value for FCY_Value if zero or NA
  if ((is.na(FCY) == TRUE | FCY == 0) & areaUnits == "ha") {
    FCY = 11.25 * area
  } else if ((is.na(FCY) == TRUE | FCY == 0) & areaUnits == "acre") {
    FCY = 4.5 * area
  }
  
  #Updated FCY to handle when area is less than 0
  if (area > 1) {
    FCY <-
      ifelse(areaUnits == "ha", FCY / area, (FCY * (2.47105 / area)))
  } else
  {
    FCY <-
      ifelse(areaUnits == "ha", FCY * area, (FCY * (2.47105 * area)))
  }
  return(FCY)
}


#response_trans #####
response_trans <- function(value){
  #convert FCY to numeric
  value = as.character(value)
  value = as.numeric(value)

  #set default value for FCY_Valu if zero or NA
  #pass actual value if not
  if (is.na(value) == TRUE | (value == 0)) {
    transformed_value = NA
  } else{
    transformed_value = value
  }
  return(transformed_value)
}


#correct_HD_input ####
correct_HD_input <- function(PD, HD){
  hd_month = as.numeric(format(HD,"%m"))
  pd_month = as.numeric(format(PD,"%m"))
  pd_year = as.numeric(format(PD,"%Y"))
  hd_year = as.numeric(format(HD,"%Y"))
  if((pd_month > hd_month) & (pd_year == hd_year)){
    HD <- as.POSIXlt(HD)
    HD$year <- HD$year + 1
    HD = as.Date(HD, format = "%Y-%m-%d")
  }
  return(HD)
}

#cassava_mandatory_maturity ####
cassava_mandatory_maturity <- function(PD,HD){
  library(lubridate)
  library(zoo)
  difference_month = (as.yearmon(strptime(HD, format = "%Y-%m-%d"))-as.yearmon(strptime(PD, format = "%Y-%m-%d")))*12
  if(difference_month < 8){
    #Add cassava should stay for 8 months 
    HD = as.Date(PD) %m+% months(8)
    return("Cassava roots must stay in ground for 8 months")
  } else if (difference_month > 15){
    #Can only it give recommendation within 15 months.
    HD = as.Date(PD) %m+% months(15)
    return("Cassava roots must not stay in ground for more than 8 months")
  } 
}

##Return area or reassign area with custom_area variable and return it
##or return invalid area input
##Expected input: area as string which can be type casted to int.
##Expected input: custom_area as string or null. Ideally the string should be able
##to be type casted to int or else function returns invalid response.
#area input manipulation ####
area_input_manipulation = function(area, custom_area) {
  ##convert area and custom area to numeric
  area = as.numeric(area)
  custom_area = as.numeric(custom_area)
  #If area > 0 (return area)
  #If area == 0 and custom_area != NA, reassign area = custom_area, and return area.
  #Else return invalid response
  if (area > 0) {
    return(area)
  } else if (area == 0 & is.na(custom_area) == F) {
    area = custom_area
    return(area)
  } else{
    return("Invalid area input")
  }
}
