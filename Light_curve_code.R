#Attach files
attach(Light_curve)

fit_AQ_curve <- function(Light_curve, group_id, Photo, PARi, fit_type = "onls"){
  AQ_curve_fits <- data.frame(ID = character(),
                              Asat = numeric(),
                              Phi = numeric(),
                              Rd = numeric(),
                              theta = numeric(),
                              resid_SSs = numeric(),
                              LCP = numeric(),
                              Q_sat_75 = numeric(),
                              Q_sat_85 = numeric(),  
                              stringsAsFactors = FALSE
  )
  if(fit_type == "onls"){
    if(require("onls")){
      print("onls is loaded correctly")
    } else {
      print("trying to install onls")
      install.packages("onls")
      if(require("onls")){
        print("onls installed and loaded")
      } else {
        stop("could not install onls")
      }
    }
    library("onls")      
    for(i in seq_along(unique(Light_curve[[group_id]]))){
      tryCatch({
        AQ_curve_fits[i, 1] <- unique(Light_curve[[group_id]])[i]
        # Subset by group_ID iteratively:
        single_curve1 <- Light_curve[Light_curve[[group_id]] == unique(Light_curve[[group_id]])[i],]
        single_curve1$assim <- single_curve1[[Photo]]
        single_curve1$PAR <- single_curve1[[PARi]]
        single_curve = single_curve1[order(single_curve1$PAR),]
        phi.as.slope <- with(single_curve,
                             as.numeric(coef(lm(
                               assim[1:5] ~ PAR[1:5]))[2]))
        # Fit the curve:
        temp.fit <- with(single_curve, # use the subset of a single curve
                         onls(assim ~ ((Phi * PAR + Asat - 
                                          sqrt((Phi * PAR + Asat)^2 - 
                                                 4 * Phi * theta * 
                                                 Asat * PAR ))
                         )/(2*theta) - Rd,
                         start=list(
                           Asat = (max(assim)),
                           Phi = phi.as.slope,
                           Rd = -min(assim),
                           theta = 0.5),
                         control = list(maxiter = 50)#,
                         #algorithm = "port"
                         )
        )
        AQ_curve_fits[i, 2] <- as.numeric(coef(temp.fit)[1]) # asat 
        AQ_curve_fits[i, 3] <- as.numeric(coef(temp.fit)[2]) # Phi
        AQ_curve_fits[i, 4] <- as.numeric(coef(temp.fit)[3]) # Rd
        AQ_curve_fits[i, 5] <- as.numeric(coef(temp.fit)[4]) # theta
        AQ_curve_fits[i, 6] <- sum(resid(temp.fit)^2)
        AQ_curve_fits[i, 7] <- (as.numeric(coef(temp.fit)[3]) *(
          as.numeric(coef(temp.fit)[3]) * as.numeric(coef(temp.fit)[4]) - 
            as.numeric(coef(temp.fit)[1]))
        ) / (as.numeric(coef(temp.fit)[2]) * (
          as.numeric(coef(temp.fit)[3]) - as.numeric(coef(temp.fit)[1])
        ))
        AQ_curve_fits[i, 8] <- (
          (as.numeric(coef(temp.fit)[1]) * 0.75 + 
             (as.numeric(coef(temp.fit)[3]))) * (
               as.numeric(coef(temp.fit)[1]) * 0.75 *
                 as.numeric(coef(temp.fit)[4]) +
                 as.numeric(coef(temp.fit)[3]) *
                 as.numeric(coef(temp.fit)[4]) -
                 as.numeric(coef(temp.fit)[1])
             )) / (
               as.numeric(coef(temp.fit)[2])* (
                 as.numeric(coef(temp.fit)[1]) * 0.75 +
                   as.numeric(coef(temp.fit)[3]) -
                   as.numeric(coef(temp.fit)[1])
               ))
        
        AQ_curve_fits[i, 9] <- (
          (as.numeric(coef(temp.fit)[1]) * 0.85 + 
             (as.numeric(coef(temp.fit)[3]))) * (
               as.numeric(coef(temp.fit)[1]) * 0.85 *
                 as.numeric(coef(temp.fit)[4]) +
                 as.numeric(coef(temp.fit)[3]) *
                 as.numeric(coef(temp.fit)[4]) -
                 as.numeric(coef(temp.fit)[1])
             )) / (
               as.numeric(coef(temp.fit)[2])* (
                 as.numeric(coef(temp.fit)[1]) * 0.85 +
                   as.numeric(coef(temp.fit)[3]) -
                   as.numeric(coef(temp.fit)[1])
               ))
      }, error = function(E){cat("Error: ", conditionMessage(E), "\n")})
    }
    return(AQ_curve_fits)
  } else{
    if(fit_type == "nls"){
      for(i in seq_along(unique(Light_curve[[group_id]]))){
        tryCatch({
          AQ_curve_fits[i, 1] <- unique(Light_curve[[group_id]])[i]
          # Subset by group_ID iteratively:
          single_curve1 <- Light_curve[Light_curve[[group_id]] == unique(Light_curve[[group_id]])[i],]
          single_curve1$assim <- single_curve1[[Photo]]
          single_curve1$PAR <- single_curve1[[PARi]]
          single_curve = single_curve1[order(single_curve1$PAR),]
          phi.as.slope <- with(single_curve,
                               as.numeric(coef(lm(
                                 assim[1:5] ~ PAR[1:5]))[2]))
          # Fit the curve:
          temp.fit <- with(single_curve, 
                           nls(assim ~ ((Phi * PAR + Asat - 
                                           sqrt((Phi * PAR + Asat)^2 - 
                                                  4 * Phi * theta * 
                                                  Asat * PAR ))
                           )/(2*theta) - Rd,
                           start=list(
                             Asat = (max(assim)),
                             Phi = phi.as.slope,
                             Rd = -min(assim),
                             theta = 0.5),
                           control = list(maxiter = 50),
                           algorithm = "port")
          )
          AQ_curve_fits[i, 2] <- as.numeric(coef(temp.fit)[1]) # asat 
          AQ_curve_fits[i, 3] <- as.numeric(coef(temp.fit)[2]) # Phi
          AQ_curve_fits[i, 4] <- as.numeric(coef(temp.fit)[3]) # Rd
          AQ_curve_fits[i, 5] <- as.numeric(coef(temp.fit)[4]) # theta
          AQ_curve_fits[i, 6] <- sum(resid(temp.fit)^2)
          AQ_curve_fits[i, 7] <- (as.numeric(coef(temp.fit)[3]) *(
            as.numeric(coef(temp.fit)[3]) * 
              as.numeric(coef(temp.fit)[4]) - 
              as.numeric(coef(temp.fit)[1]))
          ) / (as.numeric(coef(temp.fit)[2]) * (
            as.numeric(coef(temp.fit)[3]) - 
              as.numeric(coef(temp.fit)[1])
          ))
          AQ_curve_fits[i, 8] <- (
            (as.numeric(coef(temp.fit)[1]) * 0.75 + 
               (as.numeric(coef(temp.fit)[3]))) * (
                 as.numeric(coef(temp.fit)[1]) * 0.75 *
                   as.numeric(coef(temp.fit)[4]) +
                   as.numeric(coef(temp.fit)[3]) *
                   as.numeric(coef(temp.fit)[4]) -
                   as.numeric(coef(temp.fit)[1])
               )) / (
                 as.numeric(coef(temp.fit)[2])* (
                   as.numeric(coef(temp.fit)[1]) * 0.75 +
                     as.numeric(coef(temp.fit)[3]) -
                     as.numeric(coef(temp.fit)[1])
                 ))
          AQ_curve_fits[i, 9] <- (
            (as.numeric(coef(temp.fit)[1]) * 0.85 + 
               (as.numeric(coef(temp.fit)[3]))) * (
                 as.numeric(coef(temp.fit)[1]) * 0.85 *
                   as.numeric(coef(temp.fit)[4]) +
                   as.numeric(coef(temp.fit)[3]) *
                   as.numeric(coef(temp.fit)[4]) -
                   as.numeric(coef(temp.fit)[1])
               )) / (
                 as.numeric(coef(temp.fit)[2])* (
                   as.numeric(coef(temp.fit)[1]) * 0.85 +
                     as.numeric(coef(temp.fit)[3]) -
                     as.numeric(coef(temp.fit)[1])
                 ))
        }, error = function(E){
          cat("Error: ", conditionMessage(E), "\n")})
      }
      return(AQ_curve_fits)      
    } else{print("ERROR: 'fit_type' specified incorrectly.")}
  }
}

# Run the below function code and then you can fit your curves like so:
fit_AQ_curve(Light_curve, 
             Photo = "Photo", PARi = "PARi", group_id = "group_id")

# You can assign the output of this function to a variable as you would with
#     any other R function for inspection, plotting, saving

my.fits <- fit_AQ_curve(Light_curve, 
                        Photo = "Photo", PARi = "PARi", group_id = "group_id")





