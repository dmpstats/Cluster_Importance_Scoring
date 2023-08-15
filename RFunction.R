library('move2')
library('dplyr')
library('magrittr')
library('lubridate')
library('sf')
library('mgcv')

rFunction = function(data) {

  # Call provided model:
  modelfile <- paste0(getAppFilePath("providedModel"), "model.rds")
  model <- readRDS(modelfile)
  
  # prepare for modelling - introduce an upper bound to 'days'
  data %<>% mutate(response_days = pmin(days, 50))
  
  # extract the link function from the model object (m)
  ilink <- family(model)$linkinv
  
  
  # predict importance on the link scale (link because we make the confidence intervals
  # on the link scale and then back transform after)
  browser()
  
  pred <- predict(model, data, type = "link", se.fit = TRUE)
  pred <- cbind(pred, data)
  pred <- transform(pred, lwr_ci = ilink(fit - (2 * se.fit)),
                    upr_ci = ilink(fit + (2 * se.fit)),
                    fitted = ilink(fit)) %>%
    as.data.frame() %>%
    dplyr::select(fitted) %>%
    unlist()
  
  result <- data %>% mutate(importance = pred)
  
  # browser()
  # band importance score
  #bandthresh <- quantile(clust.table$importance, probs=c(0, 0.25, 0.5, 0.7, 0.8, 0.9, 1))
  bandthresh <- c(0, 0.25, 0.5, 0.75, 0.9, 1.0)
  result %<>%
    mutate(impBand = if_else(importance == 0, 0, as.numeric(cut(pred, breaks=c(bandthresh)))))
  
  
  risknames <- data.frame(impBand = c(0:5), impBandchr = c("No-feeding", "Low", "Quitelow", "Medium", "High", "Critical"))
  result <- left_join(result, risknames)
  
  
  
  
  
  
  # provide my result to the next app in the MoveApps workflow
  return(result)
}
