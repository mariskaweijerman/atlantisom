#' Calculate eaten biomass in t for each functional group.
#'
#' Function to calculate the eaten biomass in t for each functional group per
#' time, polygon, ageclass and prey item. 

##' MW Please note, that the function produces unrealistic high consumption values at the momemt.
#' @param dietcomp Dataframe containing the diet proportion for each functional group
#' per prey, time and ageclass. The dataframe has to origin from load_diet_comp using the
#' diet_check.txt as input.
##' MWDELETE NEXT TWO LINE - NOT NECESSARY TO LOAD EAT AS WELL AS GRAZING
##' MW @param eat Dataframe containing the consumed biomass in mg N for each functional-group ("species")
##' MW per timesetp, ageclass, and polygon. The dataframe has to origin from load_nc using "Eat" as select variable.
#' @param grazing Dataframe containing the consumed biomass in mg N for each functional-group ("species")
#' per timesetp, ageclass, and polygon. The dataframe has to origin from load_nc using "Grazing" as select variable.
#' @template biolprm
#' @return Dataframe in tidy format with the following columns: species, agecl, polygon, time,
#' atoutput, vol, dietcomp, prey, bio_eaten.

#' @export
#' @author Alexander Keth

calc_pred_diet <- function(dietcomp, grazing, vol, biolprm){
  dietcomp <- dplyr::filter(dietcomp, dietcomp > 0)

  # Grazing is given per species, agecl, polygon and time.
  # Therefor, we need to aggregate the volume over layers to get the consumption per polygon.
  vol <- vol %>%
    dplyr::group_by(polygon, time) %>%
    dplyr::summarise(vol = sum(atoutput))

  
  ## MW Eat is the amount of food available to eat, grazing is the amount of food acutally eaten
  ## MW we should only multiply the diet_check.txt file with the grazing file
  ## MW Combine eat and grazing! Calculate eaten biomass
  # Calculate eaten biomass per predator per age class per model run output time step (mostly annual) per model domain
  biomass_eaten <- grazing %>%
    dplyr::left_join(vol) %>%
    dplyr::left_join(dietcomp, by = c("species", "agecl", "time"))

  x_cn <- as.numeric(as.character(biolprm$redfieldcn))
  k_wetdry <- as.numeric(as.character(biolprm$kgw2d))

  # Conversion factor from mg N to t wet-weight
  bio_conv <- x_cn * k_wetdry / 1000000000

  # NOTE: Although there are observations for eat/grazing information
  # of dietcomp is missing...
  result <- biomass_eaten %>%
    dplyr::filter(!is.na(dietcomp)) %>%
    dplyr::mutate(bio_eaten = atoutput * vol * dietcomp * bio_conv)

  # NOTE: biomass eaten is given in tonnes... The values are too high! - check code if still true.

  return(result)
}



