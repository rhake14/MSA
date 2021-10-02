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

## for the adaptive test (will be added in future)
# get_eligible_first_items_MSA <- function(){
#   lower_sd <- mean(MSA::MSAa_item_bank$difficulty) - stats::sd(MSA::MSAa_item_bank$difficulty)
#   upper_sd <- mean(MSA::MSAa_item_bank$difficulty) + stats::sd(MSA::MSAa_item_bank$difficulty)
#   which(MSA::MSAa_item_bank$difficulty >= lower_sd  &
#           MSA::MSAa_item_bank$difficulty <= upper_sd)
# }


main_test <- function(label,
                      num_items,
                      audio_dir,
                      balance_over = balance_over,
                      # balance_over,
                      dict = MSA::MSA_dict
                      ) {
  elts <- c()

  ### load whole item bank and exclude practice items
  item_bank <- MSA::MSA_item_bank
  tmp_item_bank <- item_bank %>%
    dplyr::filter(practice_item == "no") %>%
    dplyr::select(-c("old_audio_file_name", "practice_item", "time_song", "set_nr"))

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
      if (((num_items / 2) + 6) / dplyr::n_distinct(tmp_item_bank %>% dplyr::filter(with_target_in_mix == "no")) <= .10) {
        probability <-  .10 # slice does not work properly for probabilities below .1
      } else {
        probability <- ((num_items / 2) + 6) / dplyr::n_distinct(tmp_item_bank %>%
                                                                   dplyr::filter(with_target_in_mix == "no"))
      }
        # for loop to get a suitable item sequence
        item_sequence_list <- NULL
        for (i in 1:100) {

          item_sequence_wit <- tmp_item_bank %>%
            dplyr::filter(with_target_in_mix == "yes") %>%
            dplyr::group_by(item_nr) %>%
            dplyr::slice_sample(n = 1) %>%
            dplyr::ungroup() %>%
            dplyr::group_by(dplyr::across(tmp_balance_over)) %>%
            dplyr::slice_sample(prop = 100) %>% # reorder the item_bank
            # dplyr::slice_sample(prop = probability) %>%
            dplyr::slice_sample(n = num_per_subgroup) %>%
            dplyr::ungroup() %>%
            dplyr::slice_sample(n = (num_items / 2))

          # difference song & item (which basically is setnumber)
          dif_song_nr <- dplyr::n_distinct(item_sequence_wit$song_nr) - num_items/2
          dif_item_nr <- dplyr::n_distinct(item_sequence_wit$item_nr) - num_items/2
          # difference conditions (alle thre conditions)
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
          dif_comp_6 <- length(which(3 == item_sequence_wit$complexity)) - num_items / 4
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

          # magicalize stuff:
          # put(
          #   item_sequence_wit,
          #   dif_song_nr,
          #   dif_item_nr,
          #   dif_overall_sum,
          #   dif_max_per_cond
          # )
        }

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

        ### WoT: item_sequence target is not in the mix
        item_sequence_wot <- tmp_item_bank %>%
          dplyr::filter(with_target_in_mix == "no") %>%
          dplyr::group_by(condition) %>%
          dplyr::slice_sample(prop = 100) %>% # just for rearrangement of the item_bank to prevent
          dplyr::slice_sample(prop = probability) %>%
          dplyr::ungroup() %>%
          dplyr::slice_sample(n = (num_items / 2))

        ### reassemble the item sequence
        item_sequence_test <- rbind(item_sequence_wot,item_sequence_wit)
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

  ### delete temporarily data
  # remove(tmp_num_cond, tmp_num_per_subgroup, probability, tmp_balance_over)

### loop through all items within the pool / item sequence--------
#
  for (i in 1:length(item_sequence)) {
    item <- MSA::MSA_item_bank[item_sequence[i],]
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
  MSA_item(label = item_id,
           correct_answer = item$correct[1],
           prompt = get_prompt(item_number, num_items),
           audio_file = item$audio_file[1],
           audio_dir = audio_dir,
           save_answer = TRUE)
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
