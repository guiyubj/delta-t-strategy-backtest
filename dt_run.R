#dt_run
#Yu Gui
#China Asset Management Co.
#Quantitative Investment Division
#7/25/2016

#run the whole process
dt_run <- function () {
  #get data
  print('Getting data...')
  dt_env$wsd_data = get_data(dt_env$str_idx, dt_env$begin_date, dt_env$end_date)$Data
  hs300_data = get_data('hs300', dt_env$begin_date, dt_env$end_date)$Data
  dt_env$excess_return_data = calc_excess_return_MAD(dt_env$wsd_data, hs300_data, 
                                              dt_env$fastMA, dt_env$slowMA)
  print('Done')
  
  
  #calc ccor_dt_matrix
  print('Calculating ccor_dt_matrix...')
  #get the list of cross-correlation matrix and delta-t matrix
  lst_mat_ccor_dt = calc_ccor_delta_t_matrix(dt_env$excess_return_data, dt_env$max_dt)
  #extract cross-correlation matrixs
  dt_env$mat_ccor = lst_mat_ccor_dt[[1]]
  #extract delta-t matrix
  dt_env$mat_dt = lst_mat_ccor_dt[[2]]
  print('Done')
  
  # #write the matrices to csv
  # str_csv_name = paste('./ccor_dt_matrix/ccor_dt_matrix_', 
  #                      dt_env$str_idx, '_dt', dt_env$max_dt, '_fast', dt_env$fastMA, '_slow', dt_env$slowMA, 
  #                      '.csv', sep = '')
  # write.csv(lst_mat_ccor_dt, str_csv_name)
  # print(paste('dt_env$mat_ccor_dt written to CSV:', str_csv_name))

  #calc trading pairs
  print('Calculating trading pairs...')
  lst_calc_trading_pairs_return = calc_trading_pairs(dt_env$mat_ccor, dt_env$mat_dt, 
                                                     dt_env$min_ccor, dt_env$min_dt, dt_env$max_dt)
  mat_tradable = lst_calc_trading_pairs_return[[1]]
  vec_trading_pairs = lst_calc_trading_pairs_return[[2]]
  lst_trading_pairs_code = lst_calc_trading_pairs_return[[3]]
  print('Done')
  
  #write tradable matrix to CSV
  str_csv_name = paste('./trading_pairs/mat_tradable_',
                       dt_env$str_idx,
                       '_dt_env$min_ccor_', dt_env$min_ccor,
                       '_dt_env$min_dt_', dt_env$min_dt, '_dt_env$max_dt_', dt_env$max_dt,
                       '_fast_', dt_env$fastMA, '_slow_', dt_env$slowMA,
                       '.csv', sep = '')
  write.csv(mat_tradable, str_csv_name)
  print(paste('mat_tradable written to CSV:', str_csv_name))
  
  # #write trading pairs list to txt
  # str_txt_name = paste('./trading_pairs/vec_trading_pairs_', 
  #                      dt_env$str_idx,
  #                      '_dt_env$min_ccor_', dt_env$min_ccor, 
  #                      '_dt_env$min_dt_', dt_env$min_dt, '_dt_env$max_dt_', dt_env$max_dt, 
  #                      '_fast_', dt_env$fastMA, '_slow_', dt_env$slowMA, 
  #                      '.txt', sep = '')
  # write(vec_trading_pairs, str_txt_name)
  # print(paste('lst_trading_pairs written to txt:', str_txt_name))
  
  
  #calc follower list
  lst_follower = calc_follower_list(dt_env$mat_ccor, dt_env$mat_dt, mat_tradable)
  
  # #write follower list to txt
  # str_txt_name = paste('./trading_pairs/lst_follower_', 
  #                      dt_env$str_idx,
  #                      '_dt_env$min_ccor_', dt_env$min_ccor, 
  #                      '_dt_env$min_dt_', dt_env$min_dt, '_dt_env$max_dt_', dt_env$max_dt, 
  #                      '_fast_', dt_env$fastMA, '_slow_', dt_env$slowMA, 
  #                      '.json', sep = '')
  # list.save(lst_follower, str_txt_name)
  # print(paste('lst_follower written to json:', str_txt_name))
  
  return(list(lst_follower, mat_tradable))
}


