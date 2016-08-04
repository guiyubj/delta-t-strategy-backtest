#bkt_start
#Yu Gui
#China Asset Management Co.
#Quantitative Investment Division
#8/2/2016

#create environment
bkt_env = new.env(parent = globalenv())

#define parameters
bkt_env$watch_time = 100
bkt_env$hold_time = 200

#generate parameters from sample data
source('./dt_start.R')
source('./dt_run.R')
bkt_env$lst_return = dt_run()

#fetch list of followers and matrix of tradable securities
bkt_env$lst_follower = bkt_env$lst_return[[1]]
bkt_env$mat_tradable = bkt_env$lst_return[[2]]


source('./backtest/bkt_account.R')
source('./backtest/bkt_signal.R')