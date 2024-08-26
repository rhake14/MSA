#source("R/MSA.R")
options(shiny.error = browser)
debug_locally <- !grepl("shiny-server", getwd())

#' Standalone MSA
#'
#' This function launches a standalone testing session for the MSA (for test batteries please use the "MSA()" function).
#' This can be used for data collection, either in the laboratory or online.

#' @param title (Scalar character) Title to display during testing.
#' @param num_items (Scalar integer) Number of items to be administered. We recommend to use at least 30 items (default)
#'  for the adaptive MSA and at least 40 for the non-adaptive version.
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
#' @param balance_over (Character vector) Indicates how items are selected from the item pool. Balance means that the proportion of items for each parameter is equal.
#' Please note that this option is only available for the non-adaptive Version of the MSA (adaptive = FALSE).
#' "target_instrument": the target instrument; balancing = equal proportion of the four different instruments ('Lead Voice', 'Piano', 'Guitar', 'Bass').
#' "complexity": the musical complexity, i.e. number of instruments within the mixture; balancing = equal proportion of items with 'three' and 'six' instruments.
#' "level": the level-ratio between target and the mixture; balancing = equal proportion of items with '0', '-5', '-10', '-15' level-ratios.
#' Default is a fully balanced design: c("target_instrument", "complexity", "level").
#' Note: By default, there is always an equal proportion of "with target instrument" and "without target" items in the pool.

#' @param take_training (Scalar boolean) Enable practice session before the actual session. Defaults to TRUE
#' @param with_picture (Scalar boolean) Includes a demonstrative picture in addition to the default text description,
#'  to be used as an explanation for the practice session (take_training needs to be TRUE). Defaults to FALSE
#'  Note: Correct display depends on the playback device. We recommend the use of Google Chrome.
#' @param with_video (Scalar boolean) Includes a demonstrative video in addition to the default text description,
#'  to be used as an explanation for the practice session (take_training needs to be TRUE). Defaults to TRUE
#'  Note: Correct display depends on the playback device. We recommend the use of Google Chrome.

#' @param with_id (Scalar boolean) Indicates, if ID should be asked for. Defaults to TRUE
#' @param with_feedback (Scalar boolean) Indicates if performance feedback will be given at the end of the test. Defaults to  FALSE
#' @param with_welcome (Scalar boolean) Indicates, if a welcome page shall be displayed. Defaults to TRUE
#' @param admin_password (Scalar character) Password for accessing the admin panel.
#' @param researcher_email (Scalar character)
#' If not \code{NULL}, this researcher's email address is displayed
#' at the bottom of the screen so that online participants can ask for help.
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include English (\code{"en"}), German (\code{"de"}); informal German (\code{"de_inf"}), formal German (\code{"de"}), and French (\code{"fr"}).
#' The first language is selected by default

#' @param validate_id (Character scalar or closure) Function for validating IDs or string "auto" for default validation
#' which means ID should consist only of  alphanumeric characters.
#' @param dict The psychTestR dictionary used for internationalisation.
#' @param ... Further arguments to be passed to \code{\link{MSA}()}.
#' @export


MSA_standalone  <- function(title = NULL,
                            num_items = 30L,
                            adaptive = TRUE,
                            long_version = FALSE,
                            balance_over = c("target_instrument", "complexity", "level"),
                            # training stuff
                            take_training = TRUE,
                            with_video = FALSE,
                            with_picture = FALSE,
                            # settings
                            with_id = TRUE,
                            with_feedback = TRUE,
                            with_welcome = TRUE,
                            location_stim = "https://media.gold-msi.org/test_materials/MSAT",
                            admin_password = "password",
                            researcher_email = "put.your.email-adress@here",
                            languages = c("en", "de","de_inf","de_f","fr"),
                            validate_id = "auto",
                            dict = MSA::MSA_dict,
                            ...) {


  feedback <- NULL
  if (with_feedback) {

    feedback <- MSA::MSA_feedback_with_graph()

    # # fix this
    # if (feedback == "graph") {
    #   feedback <-  MSA::MSA_feedback_with_graph()
    # } else {
    #   feedback <-  MSA::MSA_feedback_with_score()
    # }


  }

  elts <- psychTestR::join(
    if (with_id)
      psychTestR::new_timeline(
        psychTestR::get_p_id(prompt = psychTestR::i18n("ENTER_ID"),
                             button_text = psychTestR::i18n("CONTINUE"),
                             validate = validate_id),
        dict = dict),
    if (take_training)
      MSA::MSA(num_items = num_items,
               with_welcome =  FALSE,
               with_finish = FALSE,
               feedback = feedback,
               dict = dict,
               with_feedback = with_feedback,
               with_video = with_video,
               with_picture = with_picture,
               take_training = TRUE,
               balance_over = balance_over,
               long_version = long_version,
               location_stim = location_stim,
               adaptive = adaptive, ## future proof
               ...)
    else
      # this is to make sure that at least one instruction of the task is included
      # into the experiment
        MSA::MSA(
          num_items = num_items,
          with_welcome =  TRUE,
          with_finish = FALSE,
          feedback = feedback,
          with_feedback = with_feedback,
          dict = dict,
          take_training = FALSE,
          balance_over = balance_over,
          long_version = long_version,
          location_stim = location_stim,
          adaptive = adaptive,
          ...
        ),

    psychTestR::elt_save_results_to_disk(complete = TRUE),
    MSA_final_page(dict = dict)
  )
  if (is.null(title)) {
    #extract title as named vector from dictionary
    title <-
      MSA::MSA_dict  %>%
      as.data.frame() %>%
      dplyr::filter(key == "TESTNAME") %>%
      dplyr::select(-key) %>%
      as.list() %>%
      unlist()
    names(title) <- tolower(names(title))
  }

  psychTestR::make_test(
    elts,
    opt = psychTestR::test_options(title = title,
                                   admin_password = admin_password,
                                   researcher_email = researcher_email,
                                   demo = FALSE,
                                   languages = tolower(languages)))
}

