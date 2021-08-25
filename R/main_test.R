scoring <- function(){
  psychTestR::code_block(function(state,...){
    #browser()
    results <- psychTestR::get_results(state = state,
                                       complete = FALSE,
                                       add_session_info = FALSE) %>% as.list()
    # browser() # use browser to debug
    sum_score <- sum(purrr::map_lgl(results$MSAT, function(x) x$correct))
    num_question <- length(results$MSAT)
    perc_correct <- sum_score/num_question
    psychTestR::save_result(place = state,
                 label = "score",
                 value = perc_correct)
    psychTestR::save_result(place = state,
                             label = "num_questions",
                             value = num_question)
    })
  }

## for the adaptive test
# get_eligible_first_items_MSAT <- function(){
#   lower_sd <- mean(MSAT::MSATa_item_bank$difficulty) - stats::sd(MSAT::MSATa_item_bank$difficulty)
#   upper_sd <- mean(MSAT::MSATa_item_bank$difficulty) + stats::sd(MSAT::MSATa_item_bank$difficulty)
#   which(MSAT::MSATa_item_bank$difficulty >= lower_sd  &
#           MSAT::MSATa_item_bank$difficulty <= upper_sd)
# }

main_test <- function(label,
                      num_items,
                      audio_dir,
                      WiT = "balanced",
                      TargetIns = "balanced",
                      Complexity = "balanced",
                      LVL = "balanced",
                      unique_songs_only = TRUE,
                      dict = MSAT::MSAT_dict) {
  elts <- c()
  item_bank <- MSAT::MSAT_item_bank # load whole item ban
  tmp_item_bank <- item_bank
#### ** go through most used cases (still fishy code) -----------
##
  #### RRRR --> WiT == "random" && TargetIns == "random" && Complexity == "random" && LVL == "random"
  if(WiT == "random" && TargetIns == "random" && Complexity == "random" && LVL == "random"){
    # # count number of unique values of the item_bank for debugging ----
    # browser()
    # item_bank %>%
    #   as.tibble() %>%
    #   count(SongNr)
    # tmp_item_bank %>%
    #   as.tibble() %>%
    #   count(SongNr)
    #  end of debugging ------
    # if one want each song played max. once
    if(unique_songs_only) {tmp_item_bank <- tmp_item_bank %>% group_by(SongNr) %>% slice_sample(n = 1)}
    item_sequence <- sample(1:nrow(tmp_item_bank), num_items)
  }
  #### RBBB
  if(WiT == "random" && TargetIns == "balanced" && Complexity == "balanced" && LVL == "balanced"){
    tmp_num_cond <- length(unique(item_bank$TargetIns)) *
      length(unique(item_bank$Comp)) * length(unique(item_bank$LVL))
    tmp_num_per_subgroup <- num_items/tmp_num_cond

    # make sure we have enough items for a balanced design
    if(schoolmath::is.decimal(tmp_num_per_subgroup)){
      warning("In order to have a balanced design, the number of items in the test need be a multiple integral of the number of conditions.\n
               As a consequence, the number of items in the test have been upscaled.")
      message("The number of conditions are: ",tmp_num_cond)
      tmp_num_per_subgroup <- ceiling(tmp_num_per_subgroup)
      num_items <- tmp_num_per_subgroup * tmp_num_cond
      message("The number of items in the test are: ", num_items)
    }

    if(unique_songs_only) {tmp_item_bank <- tmp_item_bank %>% group_by(SongNr) %>% slice_sample(n = 1)}
    tmp_item_bank <- tmp_item_bank %>%
      group_by(TargetIns, Comp, LVL) %>% slice_sample(n = tmp_num_per_subgroup)
    item_sequence <- charmatch(tmp_item_bank$item_number, item_bank$item_number)
  }
  #### BBBB
  if(WiT == "balanced" && TargetIns == "balanced" && Complexity == "balanced" && LVL == "balanced"){
    item_bank <- MSAT::MSAT_item_bank
    tmp_num_cond <- length(unique(item_bank$WiT)) * length(unique(item_bank$TargetIns)) *
      length(unique(item_bank$Comp)) * length(unique(item_bank$LVL))
    tmp_num_per_subgroup <- num_items/tmp_num_cond

    # make sure we have enough items for a balanced design
    if(schoolmath::is.decimal(tmp_num_per_subgroup)){
      warning("In order to have a balanced design, the number of items in the test need be a multiple integral of the number of conditions.\n
               As a consequence, the number of items in the test have been upscaled.")
      message("The number of conditions are: ",tmp_num_cond)
      tmp_num_per_subgroup <- ceiling(tmp_num_per_subgroup)
      num_items <- tmp_num_per_subgroup * tmp_num_cond
      message("The number of items in the test are: ", num_items)
    }

    if(unique_songs_only) {tmp_item_bank <- tmp_item_bank %>% group_by(SongNr) %>% slice_sample(n = 1)}
    tmp_item_bank <- tmp_item_bank %>%
      group_by(WiT, TargetIns, Comp, LVL) %>% slice_sample(n = tmp_num_per_subgroup)
    item_sequence <- charmatch(tmp_item_bank$item_number, item_bank$item_number)
  }
  #### BBBR
  if(WiT == "balanced" && TargetIns == "balanced" && Complexity == "balanced" && LVL == "random"){
    tmp_num_cond <- length(unique(item_bank$WiT)) *
      length(unique(item_bank$TargetIns)) * length(unique(item_bank$Comp))
    tmp_num_per_subgroup <- num_items/tmp_num_cond

    # make sure we have enough items for a balanced design
    if(schoolmath::is.decimal(tmp_num_per_subgroup)){
      warning("In order to have a balanced design, the number of items in the test need be a multiple integral of the number of conditions.\n
               As a consequence, the number of items in the test have been upscaled.")
      message("The number of conditions are: ",tmp_num_cond)
      tmp_num_per_subgroup <- ceiling(tmp_num_per_subgroup)
      num_items <- tmp_num_per_subgroup * tmp_num_cond
      message("The number of items in the test are: ", num_items)
    }

    if(unique_songs_only) {tmp_item_bank <- tmp_item_bank %>% group_by(SongNr) %>% slice_sample(n = 1)}
    tmp_item_bank <- tmp_item_bank %>%
      group_by(WiT, TargetIns, Comp) %>%  slice_sample(n = tmp_num_per_subgroup)
    item_sequence <- charmatch(tmp_item_bank$item_number, item_bank$item_number)
  }
  #### BBRR
  if(WiT == "balanced" && TargetIns == "balanced" && Complexity == "random" && LVL == "random"){
    tmp_num_cond <- length(unique(item_bank$WiT)) *
      length(unique(item_bank$TargetIns))
    tmp_num_per_subgroup <- num_items/tmp_num_cond

    # make sure we have enough items for a balanced design
    if(schoolmath::is.decimal(tmp_num_per_subgroup)){
      warning("In order to have a balanced design, the number of items in the test need be a multiple integral of the number of conditions.\n
               As a consequence, the number of items in the test have been upscaled.")
      message("The number of conditions are: ",tmp_num_cond)
      tmp_num_per_subgroup <- ceiling(tmp_num_per_subgroup)
      num_items <- tmp_num_per_subgroup * tmp_num_cond
      message("The number of items in the test are: ", num_items)
    }

    if(unique_songs_only) {tmp_item_bank <- tmp_item_bank %>% group_by(SongNr) %>% slice_sample(n = 1)}
    tmp_item_bank <- tmp_item_bank %>%
      group_by(WiT, TargetIns) %>% slice_sample(n = tmp_num_per_subgroup)
    item_sequence <- charmatch(tmp_item_bank$item_number, item_bank$item_number) # get the correct indexes for the item sequence
  }
  #### BRBB
  if(WiT == "balanced" && TargetIns == "random" && Complexity == "balanced" && LVL == "balanced"){
    tmp_num_cond <- length(unique(item_bank$WiT)) * length(unique(item_bank$LVL)) *
      length(unique(item_bank$Comp))
    tmp_num_per_subgroup <- num_items/tmp_num_cond
    # make sure we have enough items for a balanced design
    if(schoolmath::is.decimal(tmp_num_per_subgroup)){
      warning("In order to have a balanced design, the number of items in the test need be a multiple integral of the number of conditions.\n
               As a consequence, the number of items in the test have been upscaled.")
      message("The number of conditions are: ",tmp_num_cond)
      tmp_num_per_subgroup <- ceiling(tmp_num_per_subgroup)
      num_items <- tmp_num_per_subgroup * tmp_num_cond
      message("The number of items in the test are: ", num_items)
    }

    if(unique_songs_only) {tmp_item_bank <- tmp_item_bank %>% group_by(SongNr) %>% slice_sample(n = 1)}
    tmp_item_bank <- tmp_item_bank %>%
      group_by(WiT, Comp, LVL) %>% slice_sample(n = tmp_num_per_subgroup)
    item_sequence <- charmatch(tmp_item_bank$item_number, item_bank$item_number) # get the correct indexes for the item sequence
  }

  #### BRBR
  if(WiT == "balanced" && TargetIns == "random" && Complexity == "balanced" && LVL == "random"){
    tmp_num_cond <- length(unique(item_bank$WiT)) * length(unique(item_bank$Comp))
    tmp_num_per_subgroup <- num_items/tmp_num_cond
    # make sure we have enough items for a balanced design
    if(schoolmath::is.decimal(tmp_num_per_subgroup)){
      warning("In order to have a balanced design, the number of items in the test need be a multiple integral of the number of conditions.\n
               As a consequence, the number of items in the test have been upscaled.")
      message("The number of conditions are: ",tmp_num_cond)
      tmp_num_per_subgroup <- ceiling(tmp_num_per_subgroup)
      num_items <- tmp_num_per_subgroup * tmp_num_cond
      message("The number of items in the test are: ", num_items)
    }

    if(unique_songs_only) {tmp_item_bank <- tmp_item_bank %>% group_by(SongNr) %>% slice_sample(n = 1)}
    tmp_item_bank <- tmp_item_bank %>%
      group_by(WiT, Comp) %>% slice_sample(n = tmp_num_per_subgroup)
    item_sequence <- charmatch(tmp_item_bank$item_number, item_bank$item_number) # get the correct indexes for the item sequence
  }

  #### BRRB
  if(WiT == "balanced" && TargetIns == "random" && Complexity == "random" && LVL == "balanced"){
    item_bank <- MSAT::MSAT_item_bank
    tmp_num_cond <- length(unique(item_bank$WiT)) * length(unique(item_bank$LVL))
    tmp_num_per_subgroup <- num_items/tmp_num_cond
    # make sure we have enough items for a balanced design
    if(schoolmath::is.decimal(tmp_num_per_subgroup)){
      warning("In order to have a balanced design, the number of items in the test need be a multiple integral of the number of conditions.\n
               As a consequence, the number of items in the test have been upscaled.")
      message("The number of conditions are: ",tmp_num_cond)
      tmp_num_per_subgroup <- ceiling(tmp_num_per_subgroup)
      num_items <- tmp_num_per_subgroup * tmp_num_cond
      message("The number of items in the test are: ", num_items)
    }

    if(unique_songs_only) {tmp_item_bank <- tmp_item_bank %>% group_by(SongNr) %>% slice_sample(n = 1)}
    tmp_item_bank <- tmp_item_bank %>%
      group_by(WiT, LVL) %>% slice_sample(n = tmp_num_per_subgroup)
    item_sequence <- charmatch(tmp_item_bank$item_number, item_bank$item_number)
  }
  #### BRRR
  if(WiT == "balanced" && TargetIns == "random" && Complexity == "random" && LVL == "random"){
    tmp_num_cond <- length(unique(item_bank$WiT)) # number of conditions
    tmp_num_per_subgroup <- num_items/tmp_num_cond # calculate how many items per subgroups should be randomly selected

    # make sure we have enough items for a balanced design
    if(schoolmath::is.decimal(tmp_num_per_subgroup)){
      warning("In order to have a balanced design, the number of items in the test need be a multiple integral of the number of conditions.\n
               As a consequence, the number of items in the test have been upscaled.")
      message("The number of conditions are: ",tmp_num_cond)
      tmp_num_per_subgroup <- ceiling(tmp_num_per_subgroup)
      num_items <- tmp_num_per_subgroup * tmp_num_cond
      message("The number of items in the test are: ", num_items)
    }

    if(unique_songs_only) {tmp_item_bank <- tmp_item_bank %>% group_by(SongNr) %>% slice_sample(n = 1)}
    tmp_item_bank <- tmp_item_bank %>%
      dplyr::group_by(WiT) %>% # selecting the subgroups as specified; number of subgroups = number of conditions
      dplyr::slice_sample(n = tmp_num_per_subgroup) # specify how many items per subgroup should be extracted
    item_sequence <- charmatch(tmp_item_bank$item_number, item_bank$item_number) # get the correct indexes for the item sequence
  }
  # delete temporarily data
  tmp_num_cond <-  NULL
  tmp_num_per_subgroup <-  NULL
  tmp_item_bank <-  NULL
# now loop through all items within the pool / item sequence--------
# browser()

  for(i in 1:length(item_sequence)){
    item <- MSAT::MSAT_item_bank[item_sequence[i],]
    # emotion <- psychTestR::i18n(item[1,]$emotion_i18)
    #printf("Emotion %s ", emotion)
    item_page <-
      MSAT_item(label = item$item_number[1],
                correct_answer = item$correct[1],
                prompt = get_prompt(i, num_items),
                audio_file = item$audio_file[1],
                audio_dir = audio_dir,
                save_answer = TRUE)
    elts <- psychTestR::join(elts, item_page)
  }
  elts
}


item_page <- function(item_number, item_id, num_items, audio_dir, dict = MSAT::MSAT_dict) {
  item <- MSAT::MSAT_item_bank %>% filter(item_number == item_id) %>% as.data.frame()
  # emotion <- psychTestR::i18n(item[1,]$emotion_i18)
  MSAT_item(label = item_id,
           correct_answer = item$correct[1],
           prompt = get_prompt(item_number, num_items),
           audio_file = item$audio_file[1],
           audio_dir = audio_dir,
           save_answer = TRUE)
  # psychTestR::audio_NAFC_page(label = sprintf("%s-%s", item_number, num_items),
  #                promp = get_prompt(item_number, num_items, emotion),
  #                choices = c("1", "2"),
  #                url = file.path(audio_dir, item$audio_file[1]))
}

get_prompt <- function(item_number, num_items, dict = MSAT::MSAT_dict) {
  shiny::div(
    shiny::h4(
      psychTestR::i18n(
        "PROGRESS_TEXT",
        sub = list(num_question = item_number,
                   test_length = if (is.null(num_items))
                     "?" else
                       num_items)),
      style  = "text_align:left"
    ),
    shiny::p(
      psychTestR::i18n("ITEM_INSTRUCTION",),
      style = "margin-left:20%;margin-right:20%;text-align:justify")
    )
}

MSAT_welcome_page <- function(dict = MSAT::MSAT_dict){
  psychTestR::new_timeline(
    psychTestR::one_button_page(
    body = shiny::div(
      shiny::h4(psychTestR::i18n("WELCOME")),
      shiny::div(psychTestR::i18n("INTRO_TEXT"),
               style = "margin-left:0%;display:block")
    ),
    button_text = psychTestR::i18n("CONTINUE")
  ), dict = dict)
}

MSAT_finished_page <- function(dict = MSAT::MSAT_dict){
  psychTestR::new_timeline(
    psychTestR::one_button_page(
      body =  shiny::div(
        shiny::h4(psychTestR::i18n("THANKS")),
        psychTestR::i18n("SUCCESS"),
                         style = "margin-left:0%;display:block"),
      button_text = psychTestR::i18n("CONTINUE")
    ), dict = dict)
}
MSAT_final_page <- function(dict = MSAT::MSAT_dict){
  psychTestR::new_timeline(
    psychTestR::final_page(
      body = shiny::div(
        shiny::h4(psychTestR::i18n("THANKS")),
        shiny::div(psychTestR::i18n("SUCCESS"),
                   style = "margin-left:0%;display:block"),
        button_text = psychTestR::i18n("CONTINUE")
      )
    ), dict = dict)
}

show_item <- function(audio_dir) {
  function(item, ...) {
    #stopifnot(is(item, "item"), nrow(item) == 1L)
    item_number <- psychTestRCAT::get_item_number(item)
    num_items <- psychTestRCAT::get_num_items_in_test(item)
    messagef("Showing item %s", item_number)
    MSAT_item(
      label = paste0("q", item_number),
      audio_file = item$audio_file,
      correct_answer = item$answer,
      # adaptive = TRUE,
      prompt = get_prompt(item_number, num_items),
      audio_dir = audio_dir,
      save_answer = TRUE,
      get_answer = NULL,
      on_complete = NULL,
      instruction_page = FALSE
    )
  }
}
