#bkt_run
#Yu Gui
#China Asset Management Co.
#Quantitative Investment Division
#8/3/2016

#initiate date
bkt_env$curr_date = bkt_env$watch_time

#initial equity
equity_long = bkt_acct$curr_equity / 2
equity_short = bkt_acct$curr_equity / 2

#start testing
while (bkt_env$curr_date < (length(dt_env$wsd_data$DATETIME) - bkt_env$hold_time)) {
  
  #refresh watching matrix to monitor past N day's return
  watch = dt_env$wsd_data[(bkt_env$curr_date - bkt_env$watch_time) : 
                               bkt_env$curr_date, 
                             2 : length(dt_env$wsd_data)
                             ]
  #calculate past N day's return
  for (i in c(1:length(watch[1,]))) {
    mat_performance[1, i] = 
      (watch[bkt_env$watch_time , i] - watch[1, i]) /
      watch[1, i]
  }
  
  #find best perfroming industry
  best_performance = max(mat_performance)
  for (i in c(1:length(mat_performance))) {
    if (mat_performance[1, i] == best_performance) {
      best_industry = colnames(mat_performance)[[i]]
    }
  }
  
  #half equity to short and half equity to long
  equity_long = bkt_acct$curr_equity / 2
  equity_short = bkt_acct$curr_equity / 2
  
  #buy the index on the follower's list of the best_industry
  lst_buy = bkt_env$lst_follower[[best_industry]]
  
  for (ticker in lst_buy) {
    share_long = (equity_long / length(lst_buy) / 
               dt_env$wsd_data[[ticker]][bkt_env$curr_date])
    buyStock(ticker, share_long)
  }
  
  #short sell the index that have soared in watch period to hedge against Beta risk
  share_short = equity_short / dt_env$wsd_data[[best_industry]][bkt_env$curr_date]
  sellShortStock(best_industry, share_short)
  
  
  #hold the portfolio firmly for a period
  bkt_env$curr_date = bkt_env$curr_date + bkt_env$hold_time
  
  refreshStock()
  
  #after the holding period
  #close out each positions opened in the portfolio
  
  #close the long positions
  for (ticker in lst_buy) {
    share_sell = bkt_acct$curr_pos[ticker, 'Shares']
    profit_long = sellStock(ticker, share_sell)[[2]]
    
    bkt_acct$curr_equity = bkt_acct$curr_equity + profit_long
  }
  
  #close the short positions
  share_sell = bkt_acct$curr_pos[best_industry, 'Shares']
  profit_short = buyBackStock(best_industry, share_sell)[[2]]
  
  #calculate total equity
  bkt_acct$curr_equity = bkt_acct$curr_equity + profit_short
  
  #record
  bkt_acct$hist_equity[length(bkt_acct$hist_equity) + 1] = 
    c(bkt_env$curr_date, bkt_acct$curr_equity)
  
  print(bkt_acct$curr_equity)
  
}

