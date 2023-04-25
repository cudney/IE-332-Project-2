tictoc::tic()
psoptim(par = rep(NA,2), fn = fit_function, lower = c(1,1), upper = c(17, 128), 
        control = list(maxit = 1000, maxit.stagnate = 10, vectorize = T, s = 100))