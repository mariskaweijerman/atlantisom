#' Approximating the true numbers at age within each box, functional group and
#' time step.
#'
#' Will be called in the run_atlantis file, and requires inputs from
#' \code{\link{load_nc}} for the "Nums" variable, and the \code{\link{load_meta}}
#' and finally the \code{\link{load_biolprm}}.
#'
#' @family calc functions
#' @author Emma Hodgson
#'
#' @template dir
#' @return A \code{data.frame} in long format with the following coumn names:
#'   Species, timestep, polygon, TRUEagecl, and atoutput (i.e., variable).
#' @export
#'
#' @examples
#' #This is just to bring in an example data frame I can use to write the code,
#' #much of the beginning code will be removed as I actually write the function,
#' #but I wanted to push what I have so far!
#' dir <- system.file("extdata", "INIT_VMPA_Jan2015", package = "atlantisom")
#' file_nc="outputSETAS.nc"
#' fgs=load_fgs(dir = dir, "functionalGroups.csv")
#' file_init="INIT_VMPA_Jan2015.nc"
#' bps=load_bps(dir = dir, fgs, file_init)
#' select_groups=fgs$Name[fgs$IsTurnedOn > 0]
#' select_variable="Nums"
#' box.info=load_box(dir = dir, file_bgm="VMPA_setas.bgm")
#' bboxes=get_boundary(box.info)
#' #when calc_stage2age is run in the run_atlantis, it will need to have the nums
#' #data frame and the bioprm already read in:
#' nums_data <- load_nc(dir = dir,
#'                      file_nc="outputSETAS.nc",
#'                      bps=bps, fgs=fgs, select_groups=select_groups,
#'                      select_variable = "Nums",
#'                      check_acronyms = TRUE, bboxes = bboxes)
#' biolprm <- load_biolprm(dir, file_biolprm="VMPA_setas_biol_fishing_Trunk.prm")

## ACTUAL FUNCTION ##
calc_stage2age <- function(dir, nums_data, biolprm) {

  # Figure out the groups that have multiple ages classes in each stage (or
  # cohort), will end up only looping over these groups
  multiple_ages <- fgs$Name[fgs$NumAgeClassSize>1]
  num_multi_age <- length(multiple_ages)

  # vector of boxes in the model, since we need to loop over boxes
  biomass_boxes = unique(nums_data$polygon)
  num_boxes = length(biomass_boxes)
  num_layers = length(unique(nums_data$layer))


  # determining the timestep output
  file.prm <- dir(dir, pattern = "run\\.prm", full.names = TRUE)
  runprm <- load_runprm(dir=, file_runprm=file.prm[1])
  timestep <- runprm

  # OKAY the above is totally broken and I am confusing myself, but what we
  # need is the number of timesteps in the model -- but I think we need them
  timestep <- 100


  # toutinc will be used for the check:
  # Here there will be some sort of if statement -- we need the output to be 365
  # days in order to ensure that recruitment is being captured at the same time
  # each year... I am not sure what to do since we need quarterly output,
  # maybe it will not matter? But I am concerned about 91 day out put since
  # we get year outputs on 364 days, which may change the measurement each year?


  # Possible method: take out all the data for one time step and do that also
  # for another time step
  # (maybe create a function that selects all the information about the timestep
  # in question -- and in that, embed the selected groups as only being those
  # that have multiple ages in a cohort)

  # Then just do matrix subtraction -- this should require only one for loop?
  # for time step...

  # Subset nums_data to only include groups that have multiple ages per stage
  multiple_ages_data <- nums_data[nums_data$species %in% multiple_ages,]

  for(i in 1:timesteps) {
    time1_dat <- multiple_ages_data[multiple_ages_data$time==(i-1)]

        # start at the second since we subtract the first
        # in this loop we will calculate for each species and cohort in each box,
        # the change in numbers between years (cohort 2 - cohort1)

        # the difference will then be divided by the stage class size for the younger
        # stage class (cohort 1)

        # we hope these will be positive -- if it is negative then we have issues


        # Then there will be an if statement to check if it is negative, and if it
        # in then we will go back in stored values to find the last positive value
        # and use that for mortality

        # And using the mortality value there is then some multiplication that
        # happens to get the final values???
        # I am brain fried and am not sure about that

      }

}
