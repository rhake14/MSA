library(tidyverse)
library(psychTestR)


#printf   <- function(...) print(sprintf(...))
#messagef <- function(...) message(sprintf(...))
#' MSAT
#'
#' This function defines a MSAT  module for incorporation into a
#' psychTestR timeline.
#' Use this function if you want to include the MSAT in a
#' battery of other tests, or if you want to add custom psychTestR
#' pages to your test timeline.
#'
#' For demoing the MSAT, consider using \code{\link{MSAT_demo}()}.
#' For a standalone implementation of the MSAT,
#' consider using \code{\link{MSAT_standalone}()}.
#' This can be used for data collection, either in the laboratory or online.

#' @param num_items (Scalar integer) Number of items to be administered.
#' @param with_welcome (Scalar boolean) Indicates, if a welcome page shall be displayed. Defaults to TRUE
#' @param take_training (Scalar boolean) Enable practice session before the actual session. Defaults to FALSE
#' @param with_finish (Scalar boolean) Indicates, if a finish (not final!) page shall be displayed. Defaults to TRUE
#' @param unique_songs_only (Scalar boolean) Set to 'TRUE' if each song (base track) should play once in the dataset.
#' Please be aware, that the item pool only consists of 98 different base tracks. Defaults to FALSE
#' @param label (Scalar character) Label to give the MSAT results in the output file.
#' @param feedback (Function) Defines the feedback to give the participant at the end of the test.
#' @param dict The psychTestR dictionary used for internationalisation.
#' @param WiT (Character scalar) With target in the mixture (WiT): Indicates how items are selected from the item pool.
#' Possible values are ("balanced") = equal proportion of items 'with target' and 'without target' in the mixture are in the item pool;
#' and ("random") = pick items randomly for this variable; Default to "balanced".
#' @param TargetIns (Character scalar) Target Instruments (TargetIns): Indicates how items are selected from the item pool.
#' Possible values are ("balanced") = equal proportion of the four different instruments ('Lead Voice', 'Piano', 'Guitar', 'Bass')
#' are selected for the item pool;
#' and ("random") = pick items randomly for this variable; Default to "random"
#' @param Complexity (Character scalar) Musical Complexity (i.e., Number of instruments within the mixture):
#' Indicates how items are selected from the item pool.
#' Possible values are ("balanced") = equal proportion of items with 'three' and 'six' instruments playing in the mixture
#' are selected for the item pool,
#' and ("random") =  pick items randomly for this variable; Default to "random"
#' @param LVL (Character scalar) Level-ratio between target and the mixture (LVL): Indicates how items are selected from the item pool.
#' Possible values are ("balanced") = equal proportion of items with '0', '-5', '-10', '-15' level-ratios are selected for the item pool,
#' and ("random") =  pick items randomly for this variable; Default to "random"
#' @export
MSAT <- function(num_items = 18L,
                with_welcome = TRUE,
                take_training = FALSE,
                with_finish = TRUE,
                unique_songs_only = FALSE,
                label = "MSAT_results",
                feedback = MSAT::MSAT_feedback_with_score(),
                dict = MSAT::MSAT_dict,
                WiT = "balanced",
                TargetIns = "random",
                Complexity = "random",
                LVL = "random") {
  audio_dir <- "https://media.gold-msi.org/test_materials/MSAT"
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
    if (with_welcome) MSAT_welcome_page(),
    psychTestR::new_timeline({
      main_test(label = label,
                num_items = num_items,
                audio_dir = audio_dir,
                dict = dict,
                unique_songs_only = FALSE,
                WiT = WiT,
                TargetIns = TargetIns,
                Complexity = Complexity,
                LVL = LVL)
    }, dict = dict),
    scoring(),
    psychTestR::elt_save_results_to_disk(complete = TRUE),
    feedback,
    if(with_finish) MSAT_finished_page(),
    psychTestR::end_module())
}
