scoring <- function(){
  psychTestR::code_block(function(state,...){
    #browser()
    results <- psychTestR::get_results(state = state,
                                       complete = FALSE,
                                       add_session_info = FALSE) %>% as.list()
    # browser() # use browser to debug
    sum_score <- sum(purrr::map_lgl(results$MSA, function(x) x$correct))
    num_question <- length(results$MSA)
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
# get_eligible_first_items_MSA <- function(){
#   lower_sd <- mean(MSA::MSAa_item_bank$difficulty) - stats::sd(MSA::MSAa_item_bank$difficulty)
#   upper_sd <- mean(MSA::MSAa_item_bank$difficulty) + stats::sd(MSA::MSAa_item_bank$difficulty)
#   which(MSA::MSAa_item_bank$difficulty >= lower_sd  &
#           MSA::MSAa_item_bank$difficulty <= upper_sd)
# }

main_test <- function(label,
                      num_items,
                      audio_dir,
                      with_target_in_mix = "balanced",
                      target_instrument = "balanced",
                      complexity = "balanced",
                      level = "balanced",
                      unique_songs_only = TRUE,
                      dict = MSA::MSA_dict) {
  elts <- c()
  item_bank <- MSA::MSA_item_bank # load whole item ban
  tmp_item_bank <- item_bank
#### ** go through most used cases (still fishy code) -----------
##
  #### RRRR --> with_target_in_mix == "random" && target_instrument == "random" && complexity == "random" && level == "random"
  if(with_target_in_mix == "random" && target_instrument == "random" && complexity == "random" && level == "random"){
    # # count number of unique values of the item_bank for debugging ----
    # browser()
    # item_bank %>%
    #   as.tibble() %>%
    #   count(song_nr)
    # tmp_item_bank %>%
    #   as.tibble() %>%
    #   count(song_nr)
    #  end of debugging ------
    # if one want each song played max. once
    if(unique_songs_only) {tmp_item_bank <- tmp_item_bank %>% group_by(song_nr) %>% slice_sample(n = 1)}
    item_sequence <- sample(1:nrow(tmp_item_bank), num_items)
  }
  #### RBBB
  if(with_target_in_mix == "random" && target_instrument == "balanced" && complexity == "balanced" && level == "balanced"){
    tmp_num_cond <- length(unique(item_bank$target_instrument)) *
      length(unique(item_bank$complexity)) * length(unique(item_bank$level))
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

    if(unique_songs_only) {tmp_item_bank <- tmp_item_bank %>% group_by(song_nr) %>% slice_sample(n = 1)}
    tmp_item_bank <- tmp_item_bank %>%
      group_by(target_instrument, complexity, level) %>% slice_sample(n = tmp_num_per_subgroup)
    item_sequence <- charmatch(tmp_item_bank$item_number, item_bank$item_number)
    item_sequence <- item_sequence[sample(1:length(item_sequence))] # now we need to randomize the sequence
  }
  #### BBBB
  if(with_target_in_mix == "balanced" && target_instrument == "balanced" && complexity == "balanced" && level == "balanced"){
    item_bank <- MSA::MSA_item_bank
    tmp_num_cond <- length(unique(item_bank$with_target_in_mix)) * length(unique(item_bank$target_instrument)) *
      length(unique(item_bank$complexity)) * length(unique(item_bank$level))
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

    if(unique_songs_only) {tmp_item_bank <- tmp_item_bank %>% group_by(song_nr) %>% slice_sample(n = 1)}
    tmp_item_bank <- tmp_item_bank %>%
      group_by(with_target_in_mix, target_instrument, complexity, level) %>% slice_sample(n = tmp_num_per_subgroup)
    item_sequence <- charmatch(tmp_item_bank$item_number, item_bank$item_number)
    item_sequence <- item_sequence[sample(1:length(item_sequence))] # now we need to randomize the sequence
  }
  #### BBBR
  if(with_target_in_mix == "balanced" && target_instrument == "balanced" && complexity == "balanced" && level == "random"){
    tmp_num_cond <- length(unique(item_bank$with_target_in_mix)) *
      length(unique(item_bank$target_instrument)) * length(unique(item_bank$complexity))
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

    if(unique_songs_only) {tmp_item_bank <- tmp_item_bank %>% group_by(song_nr) %>% slice_sample(n = 1)}
    tmp_item_bank <- tmp_item_bank %>%
      group_by(with_target_in_mix, target_instrument, complexity) %>%  slice_sample(n = tmp_num_per_subgroup)
    item_sequence <- charmatch(tmp_item_bank$item_number, item_bank$item_number)
    item_sequence <- item_sequence[sample(1:length(item_sequence))] # now we need to randomize the sequence
  }
  #### BBRR
  if(with_target_in_mix == "balanced" && target_instrument == "balanced" && complexity == "random" && level == "random"){
    tmp_num_cond <- length(unique(item_bank$with_target_in_mix)) *
      length(unique(item_bank$target_instrument))
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

    if(unique_songs_only) {tmp_item_bank <- tmp_item_bank %>% group_by(song_nr) %>% slice_sample(n = 1)}
    tmp_item_bank <- tmp_item_bank %>%
      group_by(with_target_in_mix, target_instrument) %>% slice_sample(n = tmp_num_per_subgroup)
    item_sequence <- charmatch(tmp_item_bank$item_number, item_bank$item_number) # get the correct indexes for the item sequence
    item_sequence <- item_sequence[sample(1:length(item_sequence))] # now we need to randomize the sequence
  }
  #### BRBB
  if(with_target_in_mix == "balanced" && target_instrument == "random" && complexity == "balanced" && level == "balanced"){
    tmp_num_cond <- length(unique(item_bank$with_target_in_mix)) * length(unique(item_bank$level)) *
      length(unique(item_bank$complexity))
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

    if(unique_songs_only) {tmp_item_bank <- tmp_item_bank %>% group_by(song_nr) %>% slice_sample(n = 1)}
    tmp_item_bank <- tmp_item_bank %>%
      group_by(with_target_in_mix, complexity, level) %>% slice_sample(n = tmp_num_per_subgroup)
    item_sequence <- charmatch(tmp_item_bank$item_number, item_bank$item_number) # get the correct indexes for the item sequence
    item_sequence <- item_sequence[sample(1:length(item_sequence))] # now we need to randomize the sequence
  }

  #### BRBR
  if(with_target_in_mix == "balanced" && target_instrument == "random" && complexity == "balanced" && level == "random"){
    tmp_num_cond <- length(unique(item_bank$with_target_in_mix)) * length(unique(item_bank$complexity))
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

    if(unique_songs_only) {tmp_item_bank <- tmp_item_bank %>% group_by(song_nr) %>% slice_sample(n = 1)}
    tmp_item_bank <- tmp_item_bank %>%
      group_by(with_target_in_mix, complexity) %>% slice_sample(n = tmp_num_per_subgroup)
    item_sequence <- charmatch(tmp_item_bank$item_number, item_bank$item_number) # get the correct indexes for the item sequence
    item_sequence <- item_sequence[sample(1:length(item_sequence))] # now we need to randomize the sequence
  }

  #### BRRB
  if(with_target_in_mix == "balanced" && target_instrument == "random" && complexity == "random" && level == "balanced"){
    item_bank <- MSA::MSA_item_bank
    tmp_num_cond <- length(unique(item_bank$with_target_in_mix)) * length(unique(item_bank$level))
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

    if(unique_songs_only) {tmp_item_bank <- tmp_item_bank %>% group_by(song_nr) %>% slice_sample(n = 1)}
    tmp_item_bank <- tmp_item_bank %>%
      group_by(with_target_in_mix, level) %>% slice_sample(n = tmp_num_per_subgroup)
    item_sequence <- charmatch(tmp_item_bank$item_number, item_bank$item_number)
    item_sequence <- item_sequence[sample(1:length(item_sequence))] # now we need to randomize the sequence
  }
  #### BRRR
  if(with_target_in_mix == "balanced" && target_instrument == "random" && complexity == "random" && level == "random"){
    # browser()
    tmp_num_cond <- length(unique(item_bank$with_target_in_mix)) # number of conditions
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

    if(unique_songs_only) {tmp_item_bank <- tmp_item_bank %>% group_by(song_nr) %>% slice_sample(n = 1)}
    tmp_item_bank <- tmp_item_bank %>%
      dplyr::group_by(with_target_in_mix) %>% # selecting the subgroups as specified; number of subgroups = number of conditions
      dplyr::slice_sample(n = tmp_num_per_subgroup) # specify how many items per subgroup should be extracted
    item_sequence <- charmatch(tmp_item_bank$item_number, item_bank$item_number) # get the correct indexes for the item sequence
    item_sequence <- item_sequence[sample(1:length(item_sequence))] # now we need to randomize the sequence
    }
  # delete temporarily data
  tmp_num_cond <-  NULL
  tmp_num_per_subgroup <-  NULL
  tmp_item_bank <-  NULL
# now loop through all items within the pool / item sequence--------
# browser()

  for(i in 1:length(item_sequence)){
    item <- MSA::MSA_item_bank[item_sequence[i],]
    # emotion <- psychTestR::i18n(item[1,]$emotion_i18)
    #printf("Emotion %s ", emotion)
    item_page <-
      MSA_item(label = item$item_number[1],
                correct_answer = item$correct[1],
                prompt = get_prompt(i, num_items),
                audio_file = item$audio_file[1],
                audio_dir = audio_dir,
                save_answer = TRUE)
    elts <- psychTestR::join(elts, item_page)
  }
  elts
}


item_page <- function(item_number, item_id, num_items, audio_dir, dict = MSA::MSA_dict) {
  item <- MSA::MSA_item_bank %>% filter(item_number == item_id) %>% as.data.frame()
  # emotion <- psychTestR::i18n(item[1,]$emotion_i18)
  MSA_item(label = item_id,
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

get_prompt <- function(item_number, num_items, dict = MSA::MSA_dict) {
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

MSA_welcome_page <- function(dict = MSA::MSA_dict){
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

MSA_finished_page <- function(dict = MSA::MSA_dict){
  psychTestR::new_timeline(
    psychTestR::one_button_page(
      body =  shiny::div(
        shiny::h4(psychTestR::i18n("THANKS")),
        psychTestR::i18n("SUCCESS"),
                         style = "margin-left:0%;display:block"),
      button_text = psychTestR::i18n("CONTINUE")
    ), dict = dict)
}
MSA_final_page <- function(dict = MSA::MSA_dict){
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
    MSA_item(
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
