scoring <- function(){
  psychTestR::code_block(function(state,...){

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

# for the adaptive test
get_eligible_first_items_MSA <- function(){
  # browser()
  # exclude the non valid items for the adaptive version
  valid_items <- MSA::MSA_item_bank %>%
    dplyr::filter(flagged_item == "no") %>%
    dplyr::filter(long_version_available == "yes")
  lower_sd <- mean(valid_items$difficulty) - stats::sd(valid_items$difficulty)
  upper_sd <- mean(valid_items$difficulty) + stats::sd(valid_items$difficulty)
  sample(which(valid_items$difficulty >= lower_sd  &
          valid_items$difficulty <= upper_sd),
         size = 5) # sample to randomize vector
}

main_test <- function(label,
                      num_items,
                      audio_dir,
                      balance_over = balance_over,
                      dict = MSA::MSA_dict,
                      # adaptive stuff
                      adaptive = TRUE,
                      long_version = FALSE,
                      next_item.criterion,
                      next_item.estimator,
                      next_item.prior_dist,
                      next_item.prior_par,
                      final_ability.estimator,
                      constrain_answers,
                      ...
                      ) {
  ### load whole item bank and exclude practice items

  if (adaptive) {
    if (!long_version) {item_bank <- MSA::MSA_item_bank}

    if (long_version) {
      item_bank <- MSA::MSA_item_bank %>%
        dplyr::select(-audio_file,-item_number) %>%
        dplyr::rename(audio_file = long_audio_file, # just simpler than renaming everything
                      item_number = long_item_number) %>%
        dplyr::filter(long_version_available == "yes") # some items are not available in the long format
    }
    # In the course of the calibration phase,
    # several items had to be excluded as they were found to perform poor.
    # exclude them now:
    item_bank <- item_bank %>% dplyr::filter(flagged_item == "no")
    # browser()
    psychTestRCAT::adapt_test(
      label = label,
      item_bank = item_bank,
      show_item = show_item(audio_dir, long_version),
      stopping_rule = psychTestRCAT::stopping_rule.num_items(n = num_items),
      opt = MSA_options(next_item.criterion = next_item.criterion,
                        next_item.estimator = next_item.estimator,
                        next_item.prior_dist = next_item.prior_dist,
                        next_item.prior_par = next_item.prior_par,
                        final_ability.estimator = final_ability.estimator,
                        constrain_answers = constrain_answers,
                        eligible_first_items = get_eligible_first_items_MSA(),
                        item_bank = item_bank)
    )
    # browser()


  }
  else {
  # FIX THIS: the non-adaptive MSA still uses the full item set (poor performers are not yet excluded)
    if (!long_version) {item_bank <- MSA::MSA_item_bank}

    if (long_version) {
      item_bank <- MSA::MSA_item_bank %>%
        dplyr::select(-audio_file,-item_number) %>%
        dplyr::rename(audio_file = long_audio_file,
                      item_number = long_item_number) %>%
        dplyr::filter(long_version_available == "yes")
    }

  tmp_item_bank <- item_bank %>%
    dplyr::filter(practice_item == "no") %>%
    dplyr::select(-c("old_audio_file_name", "practice_item", "time_song", "set_nr"))

  # elts <- c()
  elts <- psychTestR::code_block(function(state, ...){
    # browser()
    seed <-  psychTestR::get_session_info(state, complete = F)$p_id %>%
      digest::sha1() %>%
      charToRaw() %>%
      as.integer() %>%
      sum()
    messagef("Code block, seed %d", seed)
      ### some warnings for the user ----------------
      tmp_balance_over <-
        intersect(balance_over, c("target_instrument", "complexity", "level"))

      if (length(tmp_balance_over) != length(balance_over)) {
        warning("Found invalid balancing conditions: ",
                setdiff(balance_over, c("target_instrument", "complexity", "level")))
      }

      if (length(tmp_balance_over) == 0) {
        tmp_num_items <- max(1L, min(nrow(tmp_item_bank), num_items))
        if (nrow(tmp_item_bank) != num_items) {
          warning(sprintf(
            "%d items requested, could only retrieve %d",
            num_items,
            tmp_num_items
          ))
        }
        return(sample(1:nrow(tmp_item_bank), tmp_num_items))
      }

      num_cond <- purrr::reduce(
        tmp_balance_over,
        .f = function(x, y) {
          x * dplyr::n_distinct(tmp_item_bank[[y]])
        },
        .init = 1L
      )

      message(sprintf(
        "Found %d conditions for balancing variables: %s",
        num_cond,
        paste(tmp_balance_over, collapse = ", ")
      ))

      num_per_subgroup <- num_items / num_cond

      if (num_per_subgroup != ceiling(num_per_subgroup)) {
        # warning("Number of items does not fit subggroups, scaling up.")
        num_per_subgroup <- ceiling(num_per_subgroup)
      }

      if (num_items < 63 &  length(tmp_balance_over) == 3) {
        warning("At least 64 items must be selected for a fully balanced design.
Nevertheless, items are selected as evenly as possible with respect to the selected balancing parameters.")
      }

      ### get the item sequence ---------------
      ### preparations for the WoT item_sequence (target is not in the mix)
      if (((num_items / 2) + 10) / dplyr::n_distinct(tmp_item_bank %>% dplyr::filter(with_target_in_mix == "no")) <= .10) {
        probability <-  .10 # slice does not work properly for probabilities below .1
      }
      else if (((num_items / 2) + 10) / dplyr::n_distinct(tmp_item_bank %>% dplyr::filter(with_target_in_mix == "no")) >= 1) {
        probability <-  1 # slice does not work properly for probabilities over 1
      }
      else {
        probability <- ((num_items / 2) + 10) / dplyr::n_distinct(tmp_item_bank %>%
                                                                   dplyr::filter(with_target_in_mix == "no"))
      }
        # for loop to get a suitable item sequence
        item_sequence_list <- NULL
        # browser()
        for (i in 1:100) {

          # # FIX THIS!!! Still is not working with 100% accuracy
          item_sequence_wit <- tmp_item_bank %>%
            dplyr::filter(with_target_in_mix == "yes") %>%
            dplyr::ungroup() %>%
            dplyr::group_by(dplyr::across(tmp_balance_over)) %>%
            dplyr::slice_sample(prop = 1) %>% # reorder the item_bank
            dplyr::slice_sample(n = num_per_subgroup * 2) %>%
            dplyr::ungroup() %>%
            dplyr::group_by(item_nr) %>%
            dplyr::slice_sample(n = 1) %>%
            dplyr::ungroup() %>%
            dplyr::slice_sample(n = (num_items / 2))

          # # FIX THIS!!!: find another approach
          # item_sequence_wit <- tmp_item_bank %>%
          #   dplyr::filter(with_target_in_mix == "yes") %>%
          #   dplyr::group_by(item_nr) %>%
          #   dplyr::slice_sample(n = 1) %>%
          #   dplyr::ungroup() %>%
          #   dplyr::group_by(dplyr::across(tmp_balance_over)) %>%
          #   dplyr::slice_sample(prop = 1) %>% # reorder the item_bank
          #   # dplyr::slice_sample(prop = probability) %>%
          #   dplyr::slice_sample(n = num_per_subgroup) %>%
          #   dplyr::ungroup() %>%
          #   dplyr::slice_sample(n = (num_items / 2))

          # difference song & item (which basically is setnumber)
          dif_song_nr <- dplyr::n_distinct(item_sequence_wit$song_nr) - num_items/2
          dif_item_nr <- dplyr::n_distinct(item_sequence_wit$item_nr) - num_items/2
          # difference conditions (all three conditions)
          dif_max_per_cond <- max(plyr::count(item_sequence_wit$condition)$freq)
          dif_cond_sum <- sum(plyr::count(item_sequence_wit$condition)$freq)

          # difference level ratio
          dif_lvl_0 <- length(which(0 == item_sequence_wit$level)) - num_items / 8
          dif_lvl_5 <- length(which(-5 == item_sequence_wit$level)) - num_items / 8
          dif_lvl_10 <- length(which(-10 == item_sequence_wit$level)) - num_items / 8
          dif_lvl_15 <- length(which(-15 == item_sequence_wit$level)) - num_items / 8
          dif_lvl_sum <- sum(abs(dif_lvl_0),abs(dif_lvl_5),abs(dif_lvl_10),abs(dif_lvl_15))
          # difference acoustic complexity
          dif_comp_3 <- length(which(3 == item_sequence_wit$complexity)) - num_items / 4
          dif_comp_6 <- length(which(6 == item_sequence_wit$complexity)) - num_items / 4
          dif_comp_sum <- sum(abs(dif_comp_3),abs(dif_comp_6))
          # difference target instrument
          dif_target_lead <- length(which("Lead" == item_sequence_wit$target_instrument)) - num_items / 8
          dif_target_bass <- length(which("Bass" == item_sequence_wit$target_instrument)) - num_items / 8
          dif_target_guitar <- length(which("Guitar" == item_sequence_wit$target_instrument)) - num_items / 8
          dif_target_piano <- length(which("Piano" == item_sequence_wit$target_instrument)) - num_items / 8
          dif_target_sum <- sum(abs(dif_target_lead),abs(dif_target_bass),abs(dif_target_guitar),abs(dif_target_piano))
          # calculate overall sum & put into magical loop memory
          dif_overall_sum <- sum(dif_target_sum, dif_comp_sum, dif_lvl_sum)

          # store the output
          item_sequence_list[[i]] <-
            list(
              item_sequence_wit = item_sequence_wit,
              dif_item_nr = dif_item_nr,
              dif_song_nr = dif_song_nr,
              dif_overall_sum = dif_overall_sum
            )
        }

        # browser()
        ### extract the output from the loop
        item_sequence_wit <- tibble::as_tibble(do.call(rbind, item_sequence_list))
        item_sequence_wit <- item_sequence_wit %>%
          tidyr::unnest(c(dif_item_nr,dif_song_nr,dif_overall_sum))

        ### choose the best of the generated item sequences
        item_sequence_wit <- item_sequence_wit %>%
          dplyr::slice_max(dif_item_nr, n = 1) %>%
          dplyr::slice_min(dif_overall_sum, n = 2) %>%
          dplyr::slice_max(dif_song_nr, n = 2) %>%
          dplyr::slice_head(n = 1)

        ### get item sequence
        item_sequence_wit <- item_sequence_wit$item_sequence_wit[[1]]
        # browser()
        #
        ### WoT: item_sequence target is not in the mix
        ### FIX THIS: there is still only an approximation of equal proportion between conditions
        ### maybe while loop to fish conditions successively
        item_sequence_wot <- tmp_item_bank %>%
          dplyr::filter(with_target_in_mix == "no") %>%
          dplyr::group_by(condition) %>%
          dplyr::slice_sample(prop = 1) %>% # just for rearrangement of the item_bank to prevent
          dplyr::slice_sample(prop = probability) %>%
          dplyr::ungroup() %>%
          # dplyr::group_by(with_target_in_mix) %>%
          # dplyr::slice_sample(n = (num_items)) %>%
          # dplyr::ungroup() %>%
          # dplyr::slice_sample(prop = probability) %>%
          dplyr::slice_sample(n = (num_items / 2))

        # # get the max of equal conditions
        # dif_overall_sum <- max(plyr::count(item_sequence_wot$condition)$freq)

        ### reassemble the item sequence
        item_sequence_test <- rbind(item_sequence_wot,item_sequence_wit)
        # print(item_sequence_test)
        item_sequence <- item_sequence_test %>% dplyr::pull(item_number)

        ### for fast debugging
        # print(sprintf("Number of items input: %d", num_items))
        # print(sprintf("Number of items output: %d", nrow(item_sequence_test)))
        # item_sequence_wot %>% janitor::tabyl(condition) %>% print()
        # item_sequence_wit %>% janitor::tabyl(condition) %>% print()
        # print(sprintf("Number of unique songs in the test: %d", n_distinct(item_sequence_test$song_nr)))
        # print(sprintf("Number of unique items in the test: %d", n_distinct(item_sequence_test$item_nr)))
        # print(sprintf("Number of different items in item_sequence_wot: %d", n_distinct(item_sequence_wot$item_nr)))
        # print(sprintf("Number of different items in item_sequence_wit: %d", n_distinct(item_sequence_wit$item_nr)))
        # item_sequence_test %>% janitor::tabyl(item_nr) %>% print()
      # } # this is the else bracket

      if (length(item_sequence) != num_items) {
        warning(sprintf(
          "%d items requested, could only retrieve %d",
          num_items,
          length(item_sequence)
        ))
      }

  ### get the correct indexes for the item sequence & randomize the sequence order
  item_sequence <- charmatch(item_sequence, item_bank$item_number)
  item_sequence <- item_sequence[sample(1:length(item_sequence))]

  psychTestR::set_local(key = "item_sequence", value = item_sequence[1:num_items], state = state)
  psychTestR::set_local(key = "i_row", value = 1L, state = state)

  })
  ### delete temporarily data
  # remove(tmp_num_cond, tmp_num_per_subgroup, probability, tmp_balance_over)

### loop through all items within the pool / item sequence--------
#
  for (i_row in 1:num_items) {
    item <- psychTestR::reactive_page(function(state, ...) {
      item_sequence <- psychTestR::get_local("item_sequence", state)
      i_row <- psychTestR::get_local("i_row", state)
      # item_number <- item_sequence[i_row]
      # browser()
      # messagef("Called reactive page, i_row %d, item_number: %d", i_row, item_sequence[i_row])
      MSA_item(label = item_bank$item_number[item_sequence[i_row]],
               correct_answer = item_bank$correct[item_sequence[i_row]],
               prompt = get_prompt(i_row, num_items,long_version, audio_dir),
               audio_file = item_bank$audio_file[item_sequence[i_row]],
               audio_dir = audio_dir,
               save_answer = TRUE,
               adaptive = adaptive
               #item_number
               )
    })
    elts <- c(elts,item)
  }
  elts
  # elts <- psychTestR::join(elts, SRS_scoring(label) # vllt das hier
  }
}

# FIX THIS: is this function used somewhere?
item_page <- function(item_number, item_id, num_items, audio_dir, dict = MSA::MSA_dict) {
  item <- MSA::MSA_item_bank %>% filter(item_number == item_id) %>% as.data.frame()
  # browser()
  MSA_item(label = item_id,
           correct_answer = item$correct[1],
           prompt = get_prompt(item_number, num_items),
           audio_file = item$audio_file[1],
           audio_dir = audio_dir,
           save_answer = TRUE)
  }



# get_prompt <- function(item_number, num_items, long_version, audio_dir, dict = MSA::MSA_dict) {
#
#   if (long_version == T ) {
#     # video_url <- "http://127.0.0.1:4321/MSA_long_visual_480p.mp4"  # example offline version
#     # video_url <- "https://media.gold-msi.org/test_materials/MSAT/MSA_long_visual_480p.mp4" # example online version3
#     video_url <- paste0(audio_dir,"/MSA_long_visual_480p.mp4")
#   }
#   if (long_version == F ) {
#     # video_url <- "http://127.0.0.1:4321/MSA_visual_480p.mp4" # example offline version
#     # video_url <- "https://media.gold-msi.org/test_materials/MSAT/MSA_visual_480p.mp4" # example online version3
#     video_url <- paste0(audio_dir,"/MSA_visual_480p.mp4")
#   }
#
#   shiny::div(
#     shiny::tags$video(
#       src = video_url,
#       type = "video/mp4",
#       width = "640px",
#       height = "360px",
#       controls = "controls",
#       autoplay = "autoplay"
#     ),
#
#     shiny::h4(
#       psychTestR::i18n(
#         "PROGRESS_TEXT",
#         sub = list(num_question = item_number,
#                    test_length = if (is.null(num_items))
#                      "?" else
#                        num_items)),
#       style  = "text_align:left"
#     ),
#     shiny::p(
#       psychTestR::i18n("ITEM_INSTRUCTION",),
#       style = "margin-left:20%;margin-right:20%;text-align:justify;display:block"
#       # style = "margin-left:0%;display:block"
#     )
#   )
# }



# lets try synchronisation
get_prompt <- function(item_number, num_items, long_version, audio_dir, dict = MSA::MSA_dict) {
  if (long_version == TRUE) {
    video_url <- paste0(audio_dir, "/MSA_long_visual_480p.mp4")
    video_id <- "MSA_long_visual_480p"
  } else {
    video_url <- paste0(audio_dir, "/MSA_visual_480p.mp4")
    video_id <- "MSA_visual_480p"
  }

  shiny::div(
    shiny::tags$video(
      id = video_id,
      src = video_url,
      type = "video/mp4",
      width = "560px",
      height = "240px",
      controls = "controls"
      # Removed autoplay and insert a delay
    ),
    shiny::tags$script(
      shiny::HTML(
        sprintf("setTimeout(function() {
                    var videoElement = document.getElementById('%s');
                    videoElement.play();
                  }, 500);", video_id) #include 1 sec of delay before playing, so that video and audio have time to load and play synchron
      ) # see also get_audio_ui if you want to change this value
    ),
    shiny::h4(
      psychTestR::i18n(
        "PROGRESS_TEXT",
        sub = list(num_question = item_number,
                   test_length = if (is.null(num_items)) "?" else num_items)),
      style  = "text_align:left"
    ),
    shiny::p(
      psychTestR::i18n("ITEM_INSTRUCTION"),
      style = "margin-left:20%;margin-right:20%;text-align:justify;display:block"
    )
  )
}





MSA_welcome_page <- function(dict = MSA::MSA_dict){
  psychTestR::new_timeline(
    psychTestR::one_button_page(
    body = shiny::div(
      shiny::h4(psychTestR::i18n("WELCOME")),
      shiny::div(psychTestR::i18n("INTRO_TEXT"),
               style = "text-align: justify; margin-left:20%; margin-right:20%;display:block")
    ),
    button_text = psychTestR::i18n("CONTINUE")
  ), dict = dict)
}

MSA_picture_page_short <- function(dict = MSA::MSA_dict){
  psychTestR::new_timeline(
    psychTestR::one_button_page(
      body = shiny::div(
        shiny::h4(psychTestR::i18n("WELCOME")),
        shiny::div(psychTestR::i18n("INTRO_TEXT"),
                   style = "text-align: justify; margin-left:20%; margin-right:20%;display:block")
      ),
      button_text = psychTestR::i18n("CONTINUE")
    ), dict = dict)
}



MSA_welcome_page_long <- function(dict = MSA::MSA_dict){
  psychTestR::new_timeline(
    psychTestR::one_button_page(
      body = shiny::div(
        # shiny::h4(psychTestR::i18n("WELCOME_LONG")),
        shiny::div(psychTestR::i18n("INSTRUCTIONS_LONG"),
                   style = "text-align: justify; margin-left:20%; margin-right:20%;display:block")
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
                         style = "text-align: justify; margin-left:20%; margin-right:20%;display:block"),
      button_text = psychTestR::i18n("CONTINUE")
    ), dict = dict)
}
MSA_final_page <- function(dict = MSA::MSA_dict){
  psychTestR::new_timeline(
    psychTestR::final_page(
      body = shiny::div(
        shiny::h4(psychTestR::i18n("THANKS")),
        shiny::div(psychTestR::i18n("FINAL_P"),
                   style = "text-align: justify; margin-left:20%; margin-right:20%;display:block"),
        button_text = psychTestR::i18n("CONTINUE")
      )
    ), dict = dict)
}

show_item <- function(audio_dir, long_version) {
  function(item, ...) {
    #stopifnot(is(item, "item"), nrow(item) == 1L)
    # browser()
    item_number <- psychTestRCAT::get_item_number(item)
    num_items <- psychTestRCAT::get_num_items_in_test(item)
    # psychTestRCAT::get_current_ability_estimate()
    ### for debugging
    # browser()
    # cat("Showing item Nr: ", item_number, "=", item$item_number, "\n")
    MSA_item(
      label = paste0("sequence_order.", item_number,"_item_nr.", item$item_nr),
      audio_file = item$audio_file,
      correct_answer = item$correct,
      adaptive = TRUE,
      prompt = get_prompt(item_number, num_items, long_version, audio_dir),
      audio_dir = audio_dir,
      save_answer = TRUE,
      get_answer = NULL,
      on_complete = NULL,
      instruction_page = FALSE
    )
  }
}
