library(tidyverse)
library(psychTestR)
library(psychTestRCAT)

#' MSA
#'
#' This function defines a MSA  module for incorporation into a
#' psychTestR timeline.
#' Use this function if you want to include the MSA in a
#' battery of other tests, or if you want to add custom psychTestR
#' pages to your test timeline.
#' The MSA is an adaptive instrument to test musical scene analysis
#' abilities among very diverse populations. An non-adaptive version is also available.
#' The test is based on a two-alternative-forced-choice task - for a more
#' detailed description take a look at the Readme file on GITHUB (https://github.com/rhake14/MSA/tree/main#readme)
#'
#' For demoing the MSA, consider using \code{\link{MSA_demo}()}.
#' For a standalone implementation of the MSA,
#' consider using \code{\link{MSA_standalone}()}.
#' This can be used for data collection, either in the laboratory or online.

#' @param label (Scalar character) Label to give the MSA results in the output file.
#' @param num_items (Scalar integer) Number of items to be administered. We recommend to use at least 30 items (default)
#' for the adaptive MSA and at least 40 for the non-adaptive version.
#' @param with_welcome (Scalar boolean) Indicates, if a welcome page shall be displayed. Defaults to TRUE
#' @param with_finish (Scalar boolean) Indicates, if a finish, i.e. a completion (not final!) page shall be displayed. Defaults to TRUE

#' @param take_training (Scalar boolean) Enable practice session before the actual session. Defaults to TRUE
#' @param with_picture (Scalar boolean) Includes a demonstrative picture in addition to the default text description,
#'  to be used as an explanation for the practice session (take_training needs to be TRUE). Defaults to FALSE
#'  Note: Correct display depends on the playback device. We recommend the use of Google Chrome.
#' @param with_video (Scalar boolean) Includes a demonstrative video in addition to the default text description,
#'  to be used as an explanation for the practice session (take_training needs to be TRUE). Defaults to FALSE
#'  Note: Correct display depends on the playback device. We recommend the use of Google Chrome.

#' @param with_feedback (Scalar boolean) Defines whether the test person receives feedback at the end of the task.
#' @param feedback (Function) Defines the feedback to give the participant at the end of the test.
#' Options are "MSA::MSA_feedback_with_graph()" (Default) and "MSA::MSA_feedback_with_score()" for showing only the number of correctly detected items.

#' @param balance_over (Character vector) Indicates how items are selected from the item pool. Balance means that the proportion of items for each parameter is equal.
#' Please note that this option is only available for the non-adaptive Version of the MSA (adaptive = FALSE).
#' "target_instrument": the target instrument; balancing = equal proportion of the four different instruments ('Lead Voice', 'Piano', 'Guitar', 'Bass').
#' "complexity": the musical complexity, i.e. number of instruments within the mixture; balancing = equal proportion of items with 'three' and 'six' instruments.
#' "level": the level-ratio between target and the mixture; balancing = equal proportion of items with '0', '-5', '-10', '-15' level-ratios.
#' Default is a fully balanced design: c("target_instrument", "complexity", "level").
#' Note: By default, there is always an equal proportion of "with target instrument" and "without target" items in the pool.
#' @param dict The psychTestR (package) dictionary used for internationalisation.

#' @param adaptive (Scalar boolean) Indicates whether you want to use the adaptive MSA (TRUE)
#' or the non-adaptive MSA (FASLE). Default is adaptive = TRUE.
#' @param long_version (Scalar boolean) Indicates whether you want to use the musical excerpts that include 8 seconds of music prior to the task (TRUE):
#' If (TRUE), each trail has the following procedure:
#' Part 1: the initial music segment (8s);
#' Part 2: after 1s pause, a single instrument (lead voice, bass, guitar, or piano) plays;
#' Part 3: after 1s pause, a mix of instruments plays, possibly including the target instrument.
#' If (FALSE), Part 1 is skipped (the original version; Default).
#' @param location_stim (Scalar character) Specify the location of the stimuli.
#' Default is the location on the DOTS servers from the "Deutsche Gesellschaft für Musikpsychologie "https://media.gold-msi.org/test_materials/MSAT".
#' To start the test locally (without internet connection), one have to download all stimuli from the provided drives:
#' Stimuli long version (https://drive.google.com/drive/folders/1OI2Ii2C8yUGu8M9BzyV3HVEiKv1YFXLf?usp=sharing)
#' Stimuli short version (https://drive.google.com/drive/folders/1cxPEOyAaipXFtWNEBaBOyFkImftlEOSz?usp=sharing)
#' Videos, which should belong into the same folder as the stimuli (https://drive.google.com/drive/folders/1_za6FNNnXWREk6NRAewvXhQsn9IG3TY8?usp=sharing)
#' Then, create a local host server using e.g.,"servr" package (servr::httd("C://Users//PC NAME//Stimuli folder path")) and then provide the
#' new URL, e.g., location_stim = "http://127.0.0.1:4321".

#' @param constrain_answers (Logical scalar)
#' If \code{TRUE}, then item selection will be constrained so that the
#' correct answers are distributed as evenly as possible over the course of the test.
#' We recommend leaving this option disabled.
#' @param next_item.criterion (Character scalar)
#' Criterion for selecting successive items in the adaptive test.
#' See the \code{criterion} argument in \code{\link[catR]{nextItem}} for possible values.
#' Defaults to \code{"bOpt"}.
#' @param next_item.estimator (Character scalar)
#' Ability estimation method used for selecting successive items in the adaptive test.
#' See the \code{method} argument in \code{\link[catR]{thetaEst}} for possible values.
#' \code{"BM"}, Bayes modal,
#' corresponds to the setting used (will be further explained in the to be published MSA paper).
#' \code{"WL"}, weighted likelihood,
#' corresponds to the default setting used in versions <= 0.2.0 of this package.
#' @param next_item.prior_dist (Character scalar)
#' The type of prior distribution to use when calculating ability estimates
#' for item selection.
#' Ignored if \code{next_item.estimator} is not a Bayesian method.
#' Defaults to \code{"norm"} for a normal distribution.
#' See the \code{priorDist} argument in \code{\link[catR]{thetaEst}} for possible values.
#' @param next_item.prior_par (Numeric vector, length 2)
#' Parameters for the prior distribution;
#' see the \code{priorPar} argument in \code{\link[catR]{thetaEst}} for details.
#' Ignored if \code{next_item.estimator} is not a Bayesian method.
#' The dfeault is \code{c(0, 1)}.
#' @param final_ability.estimator
#' Estimation method used for the final ability estimate.
#' See the \code{method} argument in \code{\link[catR]{thetaEst}} for possible values.
#' The default is \code{"WL"}, weighted likelihood.
#' #' If a Bayesian method is chosen, its prior distribution will be defined
#' by the \code{next_item.prior_dist} and \code{next_item.prior_par} arguments.

#' @export


MSA <- function(label = "MSA_results",
                num_items = 30L,
                with_welcome = TRUE,
                with_finish = TRUE,
                take_training = TRUE,
                with_video = FALSE,
                with_picture = FALSE,
                with_feedback = TRUE,
                feedback = MSA::MSA_feedback_with_graph(),
                balance_over = c("target_instrument", "complexity", "level"),
                dict = MSA::MSA_dict,
                # adaptive stuff
                adaptive = TRUE,
                long_version = FALSE,
                location_stim = "https://media.gold-msi.org/test_materials/MSAT",
                next_item.criterion = "bOpt",
                next_item.estimator = "BM",
                next_item.prior_dist = "norm",
                next_item.prior_par = c(0, 1),
                final_ability.estimator = "WL",
                constrain_answers = FALSE
                ) {
  # audio_dir <- "https://media.gold-msi.org/test_materials/MSAT" # original
  # audio_dir <- "http://127.0.0.1:4321" # example local version
  audio_dir <- location_stim
  stopifnot(purrr::is_scalar_character(label),
            purrr::is_scalar_integer(num_items) || purrr::is_scalar_double(num_items),
            purrr::is_scalar_character(audio_dir),
            psychTestR::is.timeline(feedback) ||
              is.list(feedback) ||
              psychTestR::is.test_element(feedback) ||
              is.null(feedback))

  audio_dir <- gsub("/$", "", audio_dir)
  print(audio_dir)
  # label <- paste0(label,"_", stringi::stri_rand_strings(1, 3, '[A-Z]'))
  if (long_version) {
    label <- paste0(label,"_","long_", stringi::stri_rand_strings(1, 3, '[A-Z]'))
  } else {label <- paste0(label,"_","short_", stringi::stri_rand_strings(1, 3, '[A-Z]'))}


  psychTestR::join(
    psychTestR::begin_module(label), # original version

    if (take_training) {
      # browser()
      psychTestR::new_timeline(instructions(audio_dir,
                                            long_version,
                                            with_picture,
                                            with_video,
                                            take_training),
                               dict = dict)



    },

    if (take_training == F && long_version == F) MSA_welcome_page(),
    if (take_training == F && long_version == T) MSA_welcome_page_long(),


    psychTestR::new_timeline({
      # browser()
      main_test(label = label,
                num_items = num_items,
                audio_dir = audio_dir,
                dict = dict,
                balance_over = balance_over,
                # adaptive stuff
                next_item.criterion = next_item.criterion,
                next_item.estimator = next_item.estimator,
                next_item.prior_dist = next_item.prior_dist,
                next_item.prior_par = next_item.prior_par,
                final_ability.estimator = final_ability.estimator,
                constrain_answers = constrain_answers,
                long_version = long_version,
                adaptive = adaptive
                )
    }, dict = dict),

    if (!adaptive) scoring(),

    psychTestR::elt_save_results_to_disk(complete = TRUE),

    if (with_feedback) feedback,

      # Fix this
      # if (feedback == "graph") {
      #   feedback = MSA::MSA_feedback_with_graph()
      # } else {
      #   feedback = MSA::MSA_feedback_with_score()
      # }

    if (with_finish) MSA_finished_page(),
    psychTestR::end_module())
}
