library(tidyverse)
library(psychTestR)

#' MSA
#'
#' This function defines a MSA  module for incorporation into a
#' psychTestR timeline.
#' Use this function if you want to include the MSA in a
#' battery of other tests, or if you want to add custom psychTestR
#' pages to your test timeline.
#'
#' For demoing the MSA, consider using \code{\link{MSA_demo}()}.
#' For a standalone implementation of the MSA,
#' consider using \code{\link{MSA_standalone}()}.
#' This can be used for data collection, either in the laboratory or online.

#' @param num_items (Scalar integer) Number of items to be administered.
#' @param with_welcome (Scalar boolean) Indicates, if a welcome page shall be displayed. Defaults to TRUE
#' @param take_training (Scalar boolean) Enable practice session before the actual session. Defaults to FALSE
#' @param with_finish (Scalar boolean) Indicates, if a finish (not final!) page shall be displayed. Defaults to TRUE
#' @param label (Scalar character) Label to give the MSA results in the output file.
#' @param feedback (Function) Defines the feedback to give the participant at the end of the test.
#' @param dict The psychTestR dictionary used for internationalisation.
#' @param balance_over (Character vector) Indicates how items are selected from the item pool. Balance means that the proportion of items for each parameter is equal.
#' Possible parameters are
#' "target_instrument": the target instrument; balancing = equal proportion of the four different instruments ('Lead Voice', 'Piano', 'Guitar', 'Bass').
#' "complexity": the musical complexity, i.e. number of instruments within the mixture; balancing = equal proportion of items with 'three' and 'six' instruments.
#' "level": the level-ratio between target and the mixture; balancing = equal proportion of items with '0', '-5', '-10', '-15' level-ratios.
#' Default is a fully balanced design: c("target_instrument", "complexity", "level").
#' Note: By default, there is always an equal proportion of "with target instrument" and "without target" items in the pool.
#' @export
MSA <- function(num_items = 18L,
                with_welcome = TRUE,
                take_training = FALSE,
                with_finish = TRUE,
                label = "MSA_results",
                feedback = MSA::MSA_feedback_with_score(),
                dict = MSA::MSA_dict,
                balance_over = c("target_instrument", "complexity", "level")
                ) {
  audio_dir <- "https://media.gold-msi.org/test_materials/MSAT"
  # audio_dir <- "https://media.gold-msi.org/MSAT"
  stopifnot(purrr::is_scalar_character(label),
            purrr::is_scalar_integer(num_items) || purrr::is_scalar_double(num_items),
            purrr::is_scalar_character(audio_dir),
            psychTestR::is.timeline(feedback) ||
              is.list(feedback) ||
              psychTestR::is.test_element(feedback) ||
              is.null(feedback))
  audio_dir <- gsub("/$", "", audio_dir)

  psychTestR::join(
    psychTestR::begin_module(label),
    if (take_training) psychTestR::new_timeline(instructions(audio_dir),
                                                dict = dict),
    if (with_welcome) MSA_welcome_page(),
    psychTestR::new_timeline({
      main_test(label = label,
                num_items = num_items,
                audio_dir = audio_dir,
                dict = dict,
                balance_over = balance_over
                )
    }, dict = dict),
    scoring(),
    psychTestR::elt_save_results_to_disk(complete = TRUE),
    feedback,
    if (with_finish) MSA_finished_page(),
    psychTestR::end_module())
}
