#' Monte carlo test for difference in spatial covariates between test groups
#' @param dens density function column
#' @param points Points to be analysed (default is ckit)
#' @param group Factor group in hyperframe/name of column in hyperframe that determines sample group.
#' @param spatcov Spatial covariate
#' @param hf A hyperframe
#' @param geyr Geyer r variable
#' @param geysat Geyer saturation variable
#' @param nsim Number of simulations
#' @import spatstat
#' @return A monte carlo test result with p value
#' @export
#'
#' @examples montecarlo(points)

montecarlo <- function(dens = dens, points = ckit, group = group,
                       spatcov = dist_adipo, hf = hf,
                       geyr = 30, geysat = 3, nsim = 20) {

  # Assume your hyperframe is named `data` and contains:
  # - `ckit`: Point patterns (a list of point pattern objects)
  # - `group`: Design covariate (factor indicating the group)
  # - `dist_adipo`: Spatial covariate (distance to nearest fat cell)

  # Step 1: Fit the full and reduced models

  #With interaction
  model_full <- mppm(ckit ~ dens^2 + group * dist_adipo,
                                   data = hf,
                                   interaction = Geyer(30,2),
                                   random = ~1|id)

  #Without interaction
  model_red <- mppm(ckit ~ dens + group + dist_adipo + dist_vasc,
                    data = hf,
                    interaction = Geyer(30, 2),
                    random = ~1|id)

  plot(hf$ckit)
  plot(simulate(model_full, nsim = 1))
  AIC(model_full)

  # Step 2: Calculate the observed test statistic
  logL_full <- logLik(model_full, warn = FALSE)
  logL_reduced <- logLik(model_red, warn = FALSE)
  logL_full
  logL_reduced


  # Calculate the Likelihood Ratio statistic
  observed_statistic <- -2 * (logL_reduced - logL_full)

  # Degrees of freedom for the test
  df <- length(coef(model_full)) - length(coef(model_red))

  # Step 3: Simulate data under the null hypothesis

  nsim = 50
  simulated_statistics <- numeric(nsim)

  # Perform simulations
  for (i in 1:nsim) {
    print(paste("Simulation", i))
    # Simulate data under the null model
    hf$sim <- simulate(model_red)

    #With interaction
    sim_model_full <- mppm(sim ~ dens + group * dist_adipo,
                       data = hf,
                       interaction = Geyer(30, 2),
                       random = ~ 1 | id)

    #Without interaction
    sim_model_red <- mppm(sim ~ dens + group + dist_adipo,
                      data = hf,
                      interaction = Geyer(30, 2),
                      random = ~ 1 | id)

    # Compute the Likelihood Ratio statistic for simulated data
    logL_sim_full <- logLik(sim_model_full, warn = FALSE)
    logL_sim_reduced <- logLik(sim_model_red, warn = FALSE)
    logL_sim_full
    logL_sim_reduced

    simulated_statistics[i] <- -2 * (logL_sim_reduced - logL_sim_full)
  }

  # Step 4: Compute the p-value
  p_value <- mean(simulated_statistics >= observed_statistic)

  print(paste0("Result of likelihood ratio Monte Carlo test for ", i, " simulations."))
  print(paste("Observed Test Statistic:", observed_statistic))
  print(paste("P-value:", p_value))

}
