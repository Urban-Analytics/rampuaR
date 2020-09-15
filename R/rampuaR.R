#' rampuaR: A package for modelling the COVID outbreak using urban analytics
#'
#' The rampuaR package provides six main functions:
#' create_input, covid_prob, case_assign, rank_assign, infection_length,
#' removed
#'
#' @section create_input:
#' Takes the output from the microsimulation model (microsim_model.py)
#' and formats it for use in the infection model.
#'
#' @section sum_betas 
#' Summing betas for use in COVID probability calculation
#'
#' @section covid_prob:
#' Calculates the probability of each individual becoming a COVID case
#' based on the current risk they accumulate in the microsim_model.py.
#'
#' @section case_assign:
#' Assigns cases to individuals via a Bernoulli trial based on the
#' probabilities calculated in covid_prob.
#'
#' @section rank_assign:
#' Assigns cases to individuals by ranking their current risk.
#'
#' @section infection_length:
#' Assigns the number of days each COVID case is pre-symptomatic and symptomatic for.
#'
#' @section determine_removal:
#' Determines which individuals should be removed
#'
#' @section removed:
#' When individuals have reached the end of their infection they are removed.
#'
#' @section recalc_sympdays:
#' Recalculates number of symptomatic and presymptomatic days remaining
#'
#' @section run_removal_recalc
#' Runs the pipeline for removing cases and stepping symptomatic and presymptomatic days
#' 
#' #' The  package also has some functions designed to simulate specific scenarios:
#' local_outbreak
#' 
#' @section local_outbreak:
#' simulates a local outbreak or super spreader event
#' 
#' @docType package
NULL
