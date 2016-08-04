#bkt_account
#Yu Gui
#China Asset Management Co.
#Quantitative Investment Division
#8/4/2016

#create environment
bkt_acct = new.env(parent = bkt_env)

#create variables
bkt_acct$curr_equity = 100000000
bkt_acct$cash = bkt_acct$curr_equity

#create log
bkt_acct$log = list()

#create data containers in bkt_acct environment
bkt_acct$hist_equity = matrix(nrow = length(dt_env$wsd_data[[1]]), ncol = 2)
colnames(bkt_acct$hist_equity) = c('Date', 'Equity')

bkt_acct$curr_pos = data.frame(row.names = names(dt_env$wsd_data)[2:length(dt_env$wsd_data)])
for (i in c('Shares', 'PrcPerShr', 'CostPerShr', 
            'MktVal', 'Cost', 'Date', 'Profit')) {
  bkt_acct$curr_pos[[i]] = 0
}


#function: refresh stock data frame
refreshStock <- function () {
  security_worth = 0
  for (ticker in row.names(bkt_acct$curr_pos)) {
    bkt_acct$curr_pos[ticker, 'PrcPerShr'] = dt_env$wsd_data[[ticker]][bkt_env$curr_date]
    bkt_acct$curr_pos[ticker, 'MktVal'] = 
      bkt_acct$curr_pos[ticker, 'PrcPerShr'] * bkt_acct$curr_pos[ticker, 'Shares']
    security_worth = security_worth + bkt_acct$curr_pos[ticker, 'MktVal']
    
    bkt_acct$curr_pos[ticker, 'Profit'] = bkt_acct$curr_pos[ticker, 'MktVal'] - 
      sign(c(bkt_acct$curr_pos[ticker, 'Shares'])) * bkt_acct$curr_pos[ticker, 'Cost']
  }
  
  #refresh equity
  bkt_acct$curr_equity = bkt_acct$cash + security_worth
  
  bkt_acct$log = c(bkt_acct$log, list('refresh'))
}


#function: buy stock
buyStock <- function (ticker, share) {
  cash_income = 0
  profit_realized = 0;
  
  bkt_acct$curr_pos[ticker, 'Date'] = bkt_env$curr_date
  curr_price = dt_env$wsd_data[[ticker]][bkt_env$curr_date]
  bkt_acct$curr_pos[ticker, 'CostPerShr'] = 
    (bkt_acct$curr_pos[ticker, 'CostPerShr'] * bkt_acct$curr_pos[ticker, 'Shares'] + 
       curr_price * share) / 
    (bkt_acct$curr_pos[ticker, 'Shares'] + share)
  bkt_acct$curr_pos[ticker, 'Shares'] = bkt_acct$curr_pos[ticker, 'Shares'] + share
  
  #cash spending (neg)
  cash_income = -bkt_acct$curr_pos[ticker, 'CostPerShr'] * share
  bkt_acct$cash = bkt_acct$cash + cash_income
  
  #accumulate cost of purchase
  bkt_acct$curr_pos[ticker, 'Cost'] = bkt_acct$curr_pos[ticker, 'Cost'] + 
    share * curr_price
  
  bkt_acct$curr_pos[ticker, 'Profit'] = 0
  
  bkt_acct$log = c(bkt_acct$log, 
                   list(paste('buy', ticker, 'shares', 
                              share, 'cost', share * curr_price)))
  
  return(list(cash = cash_income, profit = profit_realized))
}


#function: sell stock
sellStock <- function (ticker, share) {
  cash_income = 0
  profit_realized = 0;
  
  bkt_acct$curr_pos[ticker, 'Date'] = bkt_env$curr_date
  curr_price = dt_env$wsd_data[[ticker]][bkt_env$curr_date]
  bkt_acct$curr_pos[ticker, 'PrcPerShr'] = dt_env$wsd_data[[ticker]][bkt_env$curr_date]
  bkt_acct$curr_pos[ticker, 'Shares'] = bkt_acct$curr_pos[ticker, 'Shares'] - share
  
  #cash income (positive)
  cash_income = bkt_acct$curr_pos[ticker, 'PrcPerShr'] * share
  bkt_acct$cash = bkt_acct$cash + cash_income
  
  #accumulate cost of purchase
  bkt_acct$curr_pos[ticker, 'Cost'] = bkt_acct$curr_pos[ticker, 'Cost'] -
    share * bkt_acct$curr_pos[ticker, 'CostPerShr']
  
  #take realized profit
  profit_realized = share * (bkt_acct$curr_pos[ticker, 'PrcPerShr'] - 
                               bkt_acct$curr_pos[ticker, 'CostPerShr'])
  bkt_acct$curr_pos[ticker, 'Profit'] = 
    bkt_acct$curr_pos[ticker, 'Profit'] - profit_realized
  
  bkt_acct$log = c(bkt_acct$log, 
                   list(paste('sell', ticker, 'shares', 
                              share, 'income', share * curr_price)))
  
  return(list(cash = cash_income, profit = profit_realized))
}


#function: sellShortStock
sellShortStock <- function (ticker, share) {
  cash_income = 0
  profit_realized = 0;
  
  bkt_acct$curr_pos[ticker, 'Date'] = bkt_env$curr_date
  curr_price = dt_env$wsd_data[[ticker]][bkt_env$curr_date]
  bkt_acct$curr_pos[ticker, 'CostPerShr'] = 
    (bkt_acct$curr_pos[ticker, 'CostPerShr'] * - bkt_acct$curr_pos[ticker, 'Shares'] + 
       curr_price * share) / 
    (-bkt_acct$curr_pos[ticker, 'Shares'] + share)
  bkt_acct$curr_pos[ticker, 'Shares'] = bkt_acct$curr_pos[ticker, 'Shares'] - share
  
  #cash spending (neg)
  cash_income = -bkt_acct$curr_pos[ticker, 'CostPerShr'] * share
  bkt_acct$cash = bkt_acct$cash + cash_income
  
  #accumulate cost of purchase
  bkt_acct$curr_pos[ticker, 'Cost'] = bkt_acct$curr_pos[ticker, 'Cost'] + 
    share * curr_price
  
  bkt_acct$curr_pos[ticker, 'Profit'] = 0
  
  bkt_acct$log = c(bkt_acct$log, 
                   list(paste('sellShort', ticker, 'shares', 
                              share, 'cost', share * curr_price)))
  
  return(list(cash = cash_income, profit = profit_realized))
}


#function: buyBackStock
buyBackStock <- function (ticker, share) {
  cash_income = 0
  profit_realized = 0;
  
  bkt_acct$curr_pos[ticker, 'Date'] = bkt_env$curr_date
  curr_price = dt_env$wsd_data[[ticker]][bkt_env$curr_date]
  bkt_acct$curr_pos[ticker, 'PrcPerShr'] = dt_env$wsd_data[[ticker]][bkt_env$curr_date]
  bkt_acct$curr_pos[ticker, 'Shares'] = bkt_acct$curr_pos[ticker, 'Shares'] + share
  
  #cash income (positive)
  cash_income = bkt_acct$curr_pos[ticker, 'PrcPerShr'] * -share
  bkt_acct$cash = bkt_acct$cash + cash_income
  
  #accumulate cost of purchase
  bkt_acct$curr_pos[ticker, 'Cost'] = bkt_acct$curr_pos[ticker, 'Cost'] -
    share * bkt_acct$curr_pos[ticker, 'CostPerShr']
  
  #take realized profit
  profit_realized = share * - (bkt_acct$curr_pos[ticker, 'PrcPerShr'] - 
                               bkt_acct$curr_pos[ticker, 'CostPerShr'])
  bkt_acct$curr_pos[ticker, 'Profit'] = 
    bkt_acct$curr_pos[ticker, 'Profit'] - profit_realized
  
  bkt_acct$log = c(bkt_acct$log, 
                   list(paste('buyBack', ticker, 'shares', 
                              share, 'income', share * curr_price)))
  
  return(list(cash = cash_income, profit = profit_realized))
}

