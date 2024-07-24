#' @title Model generation
#'
#' @param hf A hyperframe
#' @param geyr Geyer r variable (radius)
#' @param geysat Geyer saturation variable
#' @param all If TRUE then all models made. If FALSE only the basic model.
#' @import spatstat
#' @return Gibb's Geyer type model of the bone marrow from hyperframe of data.
#' @export
#'
#' @examples marrow.model(hf, geyr = 15, geysat = 2, all = TRUE)

marrow.model <- function(hf, geyr = 15, geysat = 2, all = TRUE){

  if(all == TRUE){

    model_full <<- mppm(ckit ~ group * dist_adipo,
                  data = hf,
                  interaction = Geyer(geyr, geysat),
                  random = ~ 1 | id)

    model_red <<- mppm(ckit ~ group + dist_adipo,
                  data = hf,
                  interaction = Geyer(geyr, geysat),
                  random = ~ 1 | id)

    model_bas <<- mppm(ckit ~ 1,
                      data = hf,
                      interaction = Geyer(a,b),
                      random = ~ 1 | id)

    model_inh <<- mppm(ckit ~ x * y,
                      data = hf,
                      interaction = Geyer(a,b),
                      random = ~ 1 | id)

  } else {

    model_bas <<- mppm(ckit ~ 1,
                      data = hf,
                      interaction = Geyer(a,b),
                      random = ~ 1 | id)

  }


  # subf <- subfits(model) #Generate subfits
  # subf_re <- subfits(model_re)
  # subf_inh <- subfits(model_inh)

}
