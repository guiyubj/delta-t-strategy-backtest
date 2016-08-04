#bkt_signal
#Yu Gui
#China Asset Management Co.
#Quantitative Investment Division
#8/4/2016

mat_performance = 
  matrix(nrow = 1, ncol = length(dt_env$wsd_data) - 1, 
         dimnames = list(c('Return'), 
                         names(dt_env$wsd_data)[2:length(dt_env$wsd_data)])
  )

