

#debugonce(fabricate_data_single_level_)
fabricate_data_single_level_(N = 10, "Y1 = rnorm(N)", "Y2 = rnorm(N)")
#debugonce(fabricate_data_)
fabricate_data_(N = 10, "Y1 = rnorm(N)", "Y2 = rnorm(N)")
#debugonce(fabricate_data)
fabricate_data(N = 10, Y1 = rnorm(N), Y2 = rnorm(N))

