#dt_start
#Yu Gui
#China Asset Management Co.
#Quantitative Investment Division
#7/21/2016

#create environment
dt_env = new.env(parent = globalenv())

#define parameters before run the program
dt_env$str_idx = 'pri' #'pri' for SYWG primary industry index, 'scd' for SYWG secondary industry index, 'wpri' for Wind industry index, 'wscd' for Wind secondary industry index

dt_env$begin_date = 'ED-5Y'
dt_env$end_date = '2011-08-01'

#define parameters before run the program
dt_env$fastMA = 10
dt_env$slowMA = 200

dt_env$min_dt = 30
dt_env$max_dt = 300 #maximum time lag between two timeseries

dt_env$min_ccor = 0.50


#import library
library(TTR)
library(rlist)
library(WindR)

#WindR
w.start()
cat('Connection status:', w.isconnected(), '\n')
source('./get_Wind_data.R')

#import functions
source('./calc_excess_return.R')
source('./calc_ccor_delta_t_matrix.R')
source('./calc_trading_pairs.R')
source('./calc_follower_list.R')

source('./dt_run.R')


