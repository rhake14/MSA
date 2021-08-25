library(tidyverse)
library(psychTestR)


#printf   <- function(...) print(sprintf(...))
#messagef <- function(...) message(sprintf(...))
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
#' @param unique_songs_only (Scalar boolean) Set to 'TRUE' if each song (base track) should play once in the dataset.
#' Please be aware, that the item pool only consists of 98 different base tracks. Defaults to FALSE
#' @param label (Scalar character) Label to give the MSA results in the output file.
#' @param feedback (Function) Defines the feedback to give the participant at the end of the test.
#' @param dict The psychTestR dictionary used for internationalisation.
#' @param with_target_in_mix (Character scalar) With target in the mixture (with_target_in_mix): Indicates how items are selected from the item pool.
#' Possible values are ("balanced") = equal proportion of items 'with target' and 'without target' in the mixture are in the item pool;
#' and ("random") = pick items randomly for this variable; Default to "balanced".
#' @param target_instrument (Character scalar) Target Instruments (TargetIns): Indicates how items are selected from the item pool.
#' Possible values are ("balanced") = equal proportion of the four different instruments ('Lead Voice', 'Piano', 'Guitar', 'Bass')
#' are selected for the item pool;
#' and ("random") = pick items randomly for this variable; Default to "random"
#' @param complexity (Character scalar) Musical complexity (i.e., Number of instruments within the mixture):
#' Indicates how items are selected from the item pool.
#' Possible values are ("balanced") = equal proportion of items with 'three' and 'six' instruments playing in the mixture
#' are selected for the item pool,
#' and ("random") =  pick items randomly for this variable; Default to "random"
#' @param level (Character scalar) Level-ratio between target and the mixture (level): Indicates how items are selected from the item pool.
#' Possible values are ("balanced") = equal proportion of items with '0', '-5', '-10', '-15' level-ratios are selected for the item pool,
#' and ("random") =  pick items randomly for this variable; Default to "random"
#' @export
MSA <- function(num_items = 18L,
                with_welcome = TRUE,
                take_training = FALSE,
                with_finish = TRUE,
                unique_songs_only = FALSE,
                label = "MSA_results",
                feedback = MSA::MSA_feedback_with_score(),
                dict = MSA::MSA_dict,
                with_target_in_mix = "balanced",
                target_instrument = "random",
                complexity = "random",
                level = "random") {
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
                unique_songs_only = FALSE,
                with_target_in_mix = with_target_in_mix,
                target_instrument = target_instrument,
                complexity = complexity,
                level = level)
    }, dict = dict),
    scoring(),
    psychTestR::elt_save_results_to_disk(complete = TRUE),
    feedback,
    if(with_finish) MSA_finished_page(),
    psychTestR::end_module())
}
