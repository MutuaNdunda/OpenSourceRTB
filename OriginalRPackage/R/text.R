####### getting intercropping text ########
getICrecText <- function(ds,country,id){
  library(stringr)
  #pull IC recommendations
  ic_rec_text = rec_text_all[rec_text_all$rec_id == 4, ]
  
  #pull and from planting practices
  and_id = rec_text_all[rec_text_all$rec_id == 2, ]
  and_id = and_id[(and_id$lang_id == id & and_id$rec_part== 30),6]
  
  if(!ds[["rec"]]$rec_F){
    
    recF <- paste0(ic_rec_text[(ic_rec_text$lang_id == id & ic_rec_text$rec_part== 6),6], ds[["rec"]]$reason_F, ".")
    
  }else{
    
    dTC <- formatC(signif(ds[["rec"]]$dTC, digits=3), format="f", big.mark=",", digits=0)
    dNR <- formatC(signif(ds[["rec"]]$dNR, digits=3), format="f", big.mark=",", digits=0)
    dMP <- signif(ds[["rec"]]$dMP, digits=2)
    currency <- ifelse(country == "NG", "NGN", "TZS")
    
    recF <- paste(ic_rec_text[(ic_rec_text$lang_id == id & ic_rec_text$rec_part== 7),6],
                   paste(formatC(round(ds[["fertilizer_rates"]]$rate, digit=0), format="f", big.mark=",", digits=0), ic_rec_text[(ic_rec_text$lang_id == id & ic_rec_text$rec_part== 8),6], ds[["fertilizer_rates"]]$type, collapse=paste0(" ", and_id, " ")),
                   paste0("."),
                   paste0(paste(ic_rec_text[(ic_rec_text$lang_id == id & ic_rec_text$rec_part== 9),6]," "), currency, " ", dTC, "."),
                   paste0(paste(ic_rec_text[(ic_rec_text$lang_id == id & ic_rec_text$rec_part== 10),6]," "), dMP), 
                   paste0(paste(ic_rec_text[(ic_rec_text$lang_id == id & ic_rec_text$rec_part== 11),6], " "), currency, " ", dNR, "."))
    
  }
  
  if (!is.null(ds[["rec"]]$reason_D)) {
    
    #trans
    recD <- ifelse(ds[["rec"]]$rec_D, ic_rec_text[(ic_rec_text$lang_id == id & ic_rec_text$rec_part== 12),6],
                   paste0(ic_rec_text[(ic_rec_text$lang_id == id & ic_rec_text$rec_part== 13),6], ds[["rec"]]$reason_D, "."))
  }else {
    
    #trans
    recD <- ifelse(ds[["rec"]]$rec_D, ic_rec_text[(ic_rec_text$lang_id == id & ic_rec_text$rec_part== 12),6],
                   paste0(word(ic_rec_text[(ic_rec_text$lang_id == id & ic_rec_text$rec_part== 13),6],1,-2), "."))
  }
  
  
  rec <- paste0(recF, recD)
  rec <- str_squish(rec)
  return(rec)
  
}

####### Returning Fertilizer Recommendation Text #####
getFRrecText <- function(ds,
                         country,
                         id
                         
){
  #Pull recommendations template
  fr_rec_text = rec_text_all[rec_text_all$rec_id == 1, ]
  if(is.null(ds)) {
    
    rec <- fr_rec_text[(fr_rec_text$lang_id == id & fr_rec_text$rec_part== 1),6]
    
  }else{
    
    if(ds[["rec"]]$TC == 0) {
      
      rec <- fr_rec_text[(fr_rec_text$lang_id == id & fr_rec_text$rec_part== 2),6]
      #TODO: This does not provide details on the reasons why we do not recommend to apply fertilizer.
      #This might either be due to
      #1. unfavourable price ratios (root price over fertilizer price is too low),
      #2 low yield potential (unfavourable planting / harvest date and low WLY),
      #3. high soil fertility and low response (high FCY or high indigenous nutrient supply).
      
      
    }else{
      
      currency <- ifelse(country == "NG", "NGN", "TZS")
      TC <- formatC(signif(ds[["rec"]]$TC, digits=3), format="f", big.mark=",", digits=0)
      NR <- formatC(signif(ds[["rec"]]$cropSale - ds[["rec"]]$TC, digits=3), format="f", big.mark=",", digits=0)
      DY <- signif(ds[["rec"]]$DY, digits=2)
      ds[["fertilizer_rates"]]$rate <- signif(ds[["fertilizer_rates"]]$rate, digits = 2)
      
      rec <- paste0(fr_rec_text[(fr_rec_text$lang_id == id & fr_rec_text$rec_part== 3),6]," ",
                    paste0(ds[["fertilizer_rates"]]$rate, " ",fr_rec_text[(fr_rec_text$lang_id == id & fr_rec_text$rec_part== 4),6]," ", ds[["fertilizer_rates"]]$type, collapse=", "),
                    " ",fr_rec_text[(fr_rec_text$lang_id == id & fr_rec_text$rec_part== 5),6],DY, " ", fr_rec_text[(fr_rec_text$lang_id == id & fr_rec_text$rec_part== 6),6], ". ",
                    "",fr_rec_text[(fr_rec_text$lang_id == id & fr_rec_text$rec_part== 7),6], " ", currency, " ", TC, ",",
                    " ",fr_rec_text[(fr_rec_text$lang_id == id & fr_rec_text$rec_part== 8),6], " ", currency, " ", NR, ".")
      
      #TODO: This only provides the minimal information to return to the user. We may consider adding following information:
      #1. Split regime - how should this fertilizer application be distributed over time?
      #2. Best application method - furrow or full ring application.
      #3. Possible better alternative fertilizers...
      #4. Importance of good agronomic practices
      #5. Possible issues with the input data - very high fertilizer prices or very low root price, very low or very high FCY, very low or very high WY,...
      
    }
  }
  
  return(rec)
}

####### Returning Scheduled Planting Recommendation Text #########
getSPrecText <- function(ds,country, id){
  library(stringr)
  #Pull sp recommendation.
  sp_rec_text = rec_text_all[rec_text_all$rec_id == 3, ]
  
  if(is.null(ds)) {
    
    rec <- sp_rec_text[(sp_rec_text$lang_id == id & sp_rec_text$rec_part== 1),6]
    
  }else{
    
    if(ds[1,]$CP) {
      
      rec <- paste0(sp_rec_text[(sp_rec_text$lang_id == id & sp_rec_text$rec_part== 2),6], format(ds[1,]$PD, "%d %B %Y"),
                    sp_rec_text[(sp_rec_text$lang_id == id & sp_rec_text$rec_part== 3),6], format(ds[1,]$HD, "%d %B %Y"), " ",
                    sp_rec_text[(sp_rec_text$lang_id == id & sp_rec_text$rec_part== 4),6])
      #TODO: This does not provide details on the reasons. This might either be due to
      #1. unfavourable price conditions at other planting/harvest dates,
      #2. low starch content at later harvest dates (if selling to as starch factory)
      #3. unattractive yields (at earlier harvest dates),
      #4. combination of both.
      #We may also want to include some information on the impact on cropping practices, requirement to ridge, risks of pest and disease issues,...
      
    }else{
      
      if(ds[1,]$PD != ds[ds$CP==TRUE,]$PD){
        recP <- paste(sp_rec_text[(sp_rec_text$lang_id == id & sp_rec_text$rec_part== 5),6], format(ds[1,]$PD, "%d %B %Y"), ", ",
                       abs(ds[1,]$rPWnr), sp_rec_text[(sp_rec_text$lang_id == id & sp_rec_text$rec_part== 6),6], ifelse(ds[1,]$rPWnr<0, sp_rec_text[(sp_rec_text$lang_id == id & sp_rec_text$rec_part== 7),6], sp_rec_text[(sp_rec_text$lang_id == id & sp_rec_text$rec_part== 8),6]), sp_rec_text[(sp_rec_text$lang_id == id & sp_rec_text$rec_part== 9),6])
      }else{
        recP <- NULL
      }
      
      if(ds[1,]$HD != ds[ds$CP==TRUE,]$HD){
        recH <-
          paste0(
            sp_rec_text[(sp_rec_text$lang_id == id &
                           sp_rec_text$rec_part == 10), 6],
            format(ds[1, ]$HD, "%d %B %Y"),
            ",",
            paste("", abs(ds[1, ]$rHWnr)),
            paste("", sp_rec_text[(sp_rec_text$lang_id == id &
                                     sp_rec_text$rec_part == 6), 6]),
            ifelse(ds[1, ]$rHWnr < 0, sp_rec_text[(sp_rec_text$lang_id == id &
                                                               sp_rec_text$rec_part == 7), 6], sp_rec_text[(sp_rec_text$lang_id == id &
                                                                                                                         sp_rec_text$rec_part == 9), 6])
            # str_to_title(sp_rec_text[(sp_rec_text$lang_id == id &
            #                          sp_rec_text$rec_part == 11), 6])
          )
      }else{
        recH <- NULL
      }
      
      DP  <- signif(ds[1, ]$RP - ds[ds$CP==TRUE,]$RP, digits=2)
      currency <- ifelse(country == "NG", "NGN", "TZS")
      dGR <- formatC(signif(ds[1, ]$dGR, digits=3), format="f", big.mark=",", digits=0)
      
      if(DP == 0){
        
        if(dGR == 0){
          recR <- paste(sp_rec_text[(sp_rec_text$lang_id == id & sp_rec_text$rec_part== 12),6],
                         ifelse(!is.null(recH), sp_rec_text[(sp_rec_text$lang_id == id & sp_rec_text$rec_part== 13),6], sp_rec_text[(sp_rec_text$lang_id == id & sp_rec_text$rec_part== 14),6]))
        }else{
          recR <- paste(sp_rec_text[(sp_rec_text$lang_id == id & sp_rec_text$rec_part== 15),6],
                         currency, " ", dGR, sp_rec_text[(sp_rec_text$lang_id == id & sp_rec_text$rec_part== 16),6])
        }
        
      }else{
        
        if(dGR == 0){
          recR <- paste(sp_rec_text[(sp_rec_text$lang_id == id & sp_rec_text$rec_part== 17),6], ifelse(DP<0, sp_rec_text[(sp_rec_text$lang_id == id & sp_rec_text$rec_part== 18),6], sp_rec_text[(sp_rec_text$lang_id == id & sp_rec_text$rec_part== 19),6]), sp_rec_text[(sp_rec_text$lang_id == id & sp_rec_text$rec_part== 20),6], abs(DP), sp_rec_text[(sp_rec_text$lang_id == id & sp_rec_text$rec_part== 21),6],
                         sp_rec_text[(sp_rec_text$lang_id == id & sp_rec_text$rec_part== 21),6],
                         ifelse(!is.null(recH), sp_rec_text[(sp_rec_text$lang_id == id & sp_rec_text$rec_part== 13),6], sp_rec_text[(sp_rec_text$lang_id == id & sp_rec_text$rec_part== 14),6]))
        }else{
          recR <- paste(sp_rec_text[(sp_rec_text$lang_id == id & sp_rec_text$rec_part== 17),6], ifelse(DP<0, sp_rec_text[(sp_rec_text$lang_id == id & sp_rec_text$rec_part== 18),6], sp_rec_text[(sp_rec_text$lang_id == id & sp_rec_text$rec_part== 19),6]), sp_rec_text[(sp_rec_text$lang_id == id & sp_rec_text$rec_part== 20),6], abs(DP), sp_rec_text[(sp_rec_text$lang_id == id & sp_rec_text$rec_part== 21),6],
                         ifelse(DP<0, sp_rec_text[(sp_rec_text$lang_id == id & sp_rec_text$rec_part== 23),6], sp_rec_text[(sp_rec_text$lang_id == id & sp_rec_text$rec_part== 24),6]),
                         sp_rec_text[(sp_rec_text$lang_id == id & sp_rec_text$rec_part== 25),6], currency, " ", dGR, ".")
        }
        
      }
      
      rec <- paste(recP, recH, recR)
      #rec <- str_squish(rec)
      
      #TODO: This only provides the minimal information to return to the user. We may consider adding following information:
      #1. Risks of harvesting later (especially in CBSD-affected areas)
      #2. Importance of using the right varieties
      #3. Reasons underlying recommendations (driven by yield, price or both)
      #4. Implications on agronomic practices, requirements for ridging, fertilizer application,...
      #5. Possible issues with the input data - especially if user provides unrealistic prices.
      
    }
    
  }
  rec <- str_squish(rec)
  return(rec)
}

####### Return PP recommendation text #########
getPPText <- function(ds,country, id){
  library(stringr)
  #We may want to change method_ploughing and method_ridging to character in the getPPrecommendations function?
  ds$method_ploughing <- as.character(ds$method_ploughing)
  ds$method_ridging   <- as.character(ds$method_ridging)
  
  pp_rec_text = rec_text_all[rec_text_all$rec_id == 2, ]
  
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


####### Returning Intercropping Tanzania #########
getCISrecText <- function(ds, country, id){
  library(stringr)
  #Get all IC intercropping text
  ic_rec_text = rec_text_all[rec_text_all$rec_id == 5,]
  
  if (!ds[["rec"]]$rec_IC) {
    recIC <-ic_rec_text[(ic_rec_text$lang_id == id & ic_rec_text$rec_part== 1),6]
      #"Intercropping is not recommended. Growing a cassava monocrop will give you a higher profit."
    
    recF  <-ic_rec_text[(ic_rec_text$lang_id == id & ic_rec_text$rec_part== 2),6]
      #"If you consider investing in fertilizer, please use our Fertilizer Recommendations Tool to obtain fertilizer advice for a cassava monocrop."
    
    
  } else{
    recIC <- ic_rec_text[(ic_rec_text$lang_id == id & ic_rec_text$rec_part== 3),6]
      #"Intercropping your cassava with sweet potato is recommended. This will generate a higher profit overall, and also give you access to early income from sweet potato."
    
    if (!ds[["rec"]]$rec_F) {
      recF <-
        paste0(ic_rec_text[(ic_rec_text$lang_id == id & ic_rec_text$rec_part== 4),6],
               ds[["rec"]]$reason_F,
               ".")
      
    } else{
      dTC <-
        formatC(
          signif(ds[["rec"]]$dTC, digits = 3),
          format = "f",
          big.mark = ",",
          digits = 0
        )
      
      dNR <-
        formatC(
          signif(ds[["rec"]]$dNR, digits = 3),
          format = "f",
          big.mark = ",",
          digits = 0
        )
      
      currency <- "TZS"
      
      
      
      recF <- paste0(
        ic_rec_text[(ic_rec_text$lang_id == id & ic_rec_text$rec_part== 8),6],
        
        paste0(round(ds[["fertilizer_rates"]]$rate), ic_rec_text[(ic_rec_text$lang_id == id & ic_rec_text$rec_part== 9),6], ds[["fertilizer_rates"]]$type, collapse =
                 ""),
        
        ic_rec_text[(ic_rec_text$lang_id == id & ic_rec_text$rec_part== 10),6],
        
        ic_rec_text[(ic_rec_text$lang_id == id & ic_rec_text$rec_part== 12),6],
        currency,
        " ",
        dTC,
        ". ",
        
        ic_rec_text[(ic_rec_text$lang_id == id & ic_rec_text$rec_part== 11),6],
        currency,
        " ",
        dNR,
        ic_rec_text[(ic_rec_text$lang_id == id & ic_rec_text$rec_part== 10),6]
      )
      
    }
  }
  
  
  rec <- paste(recIC, recF)
  rec <- str_squish(rec)
    return(rec)
}
