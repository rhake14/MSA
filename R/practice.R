#### Version 1 ----------------------------------------------------------------

# ### example stimuli for practice/training session
# sample_data <- list(
#   label =
#     c(
#       "voc_Item3567_set948_wit_lvl0_comp6_t230_song14",
#       "b_Item501_set206_wit_lvl0_comp3_t310_song18",
#       "git_Item1536_set469_wit_lvl-15_comp3_t158_song46",
#       "voc_Item2436_set462_wit_lvl0_comp3_t350_song5",
#       "voc_Item2330_set427_wot_lvl0_comp7_t100_song19-exception",
#       "voc_Item3228_set786_wot_lvl0_comp4_t32_song16"
#     ),
#   sample_audios =
#     c(
#       "voc_Item3567_set948_wit_lvl0_comp6_t230_song14.wav",
#       "b_Item501_set206_wit_lvl0_comp3_t310_song18.wav",
#       "git_Item1536_set469_wit_lvl-15_comp3_t158_song46.wav",
#       "voc_Item2436_set462_wit_lvl0_comp3_t350_song5.wav",
#       "voc_Item2330_set427_wot_lvl0_comp7_t100_song19.wav", # this one is actually with one included - wrong naming..
#       "voc_Item3228_set786_wot_lvl0_comp4_t32_song16.wav"
#     ),
#   sample_audios_answers = c(1, 1, 1, 1, 1, 2) # the naming error is reflected here
# ) %>% as_tibble() %>% dplyr::slice_sample(n = 6) %>%
#   as.list() # randomize list





ask_repeat <- function(prompt) {
  # browser()
  psychTestR::NAFC_page(
    label = "ask_repeat",
    prompt = prompt,
    choices = c("go_back", "continue"),
    labels = lapply(c("GOBACK", "CONTINUE"), psychTestR::i18n),
    save_answer = FALSE,
    arrange_vertically = FALSE,
    on_complete = function(state, answer, ...) {
      psychTestR::set_local("do_intro", identical(answer, "go_back"), state)
    }
  )
  # browser()
}

make_practice_page <- function(page_no, audio_dir) {
  psychTestR::reactive_page(function(answer, ...) {
    # browser()
    correct <- "INCORRECT"
    if (page_no > 1) {
      answer <- answer %>%
        dplyr::mutate(correct = dplyr::case_when(
          answer == 1 & stringr::str_sub(label, -1) == "Y" ~ TRUE,
          answer == 1 & stringr::str_sub(label, -1) == "N" ~ FALSE,
          answer == 2 & stringr::str_sub(label, -1) == "N" ~ TRUE,
          answer == 2 & stringr::str_sub(label, -1) == "Y" ~ FALSE,

          answer == 1 & stringr::str_detect(label, "wit")  ~ TRUE,
          answer == 1 & stringr::str_detect(label, "wot")   ~ FALSE,
          answer == 2 & stringr::str_detect(label, "wit")  ~ FALSE,
          answer == 2 & stringr::str_detect(label, "wot")  ~ TRUE,

          answer == 1 & stringr::str_detect(label, "WiT")  ~ TRUE,
          answer == 1 & stringr::str_detect(label, "WoT")  ~ FALSE,
          answer == 2 & stringr::str_detect(label, "WiT")  ~ FALSE,
          answer == 2 & stringr::str_detect(label, "WoT")  ~ TRUE,

          TRUE ~ FALSE  # Default case
        ))
      answer = answer[1,]} # Fix this: gets "NA" otherswise...
    if (page_no > 1 && answer$correct) {
      correct <- "CORRECT"}
    feedback <- psychTestR::i18n(correct)
    get_practice_page(page_no, feedback, audio_dir)
    # browser()
  })
}

# shiny::div(
#   shiny::div(psychTestR::i18n("SAMPLE1b"),
#              style = "text-align: justify; margin-left:20%;
#                margin-right:20%; margin-bottom:20px; display:block"),
#   shiny::p(audio)
# )


# i want to change this here # ------------------------------------------------
#
# get_practice_page <- function(page_no, feedback, audio_dir) {
#   # browser()
#   key <- sprintf("PRACTICE%d", page_no)
#   if (page_no == 5) {
#     # browser()
#     key <- "TRANSITION"
#   }
#   prompt <- shiny::div(
#     psychTestR::i18n(key, html = T, sub = list(feedback = feedback)),
#     style = "text-align: justify; margin-left:20%;margin-right:20%; margin-bottom:20px; display:block")
#
#   if (page_no == 5) {
#     page <- ask_repeat(prompt)
#   }
#   else{
#     # browser()
#     page <- MSA_item(
#       # label = sprintf("training%s", page_no),
#       label = sample_data$label[[page_no]],
#       correct_answer = sample_data$sample_audios_answers[page_no],
#       prompt = prompt,
#       audio_dir = audio_dir,
#       audio_file = sample_data$sample_audios[[page_no]],
#       adaptive = FALSE,
#       # adaptive = TRUE,
#       save_answer = FALSE,
#       instruction_page = FALSE
#     )
#
#   }
#   # browser()
#   page
# }
#
# practice <- function(audio_dir) {
#   lapply(1:5, make_practice_page, audio_dir) %>% unlist()
# }


# get_practice_page <- function(page_no, feedback, audio_dir) {
#   # browser()
#   key <- sprintf("PRACTICE%d", page_no)
#   if (page_no == 5) {
#     # browser()
#     key <- "TRANSITION"
#   }
#   # create the text each trial:
#   # insert the feedback / "Your response was correct"
#   # this prompt only for transition (without the video)
#   prompt <- shiny::div(
#     psychTestR::i18n(key, html = T, sub = list(feedback = feedback)),
#     style = "text-align: justify; margin-left:20%;margin-right:20%; margin-bottom:20px; display:block")
#
#   if (page_no != 5) {
#     # include video as well
#     video_url <- paste0(audio_dir, "/MSA_visual_480p.mp4")
#     video_id <- "MSA_visual_480p"
#     # create the text each trial
#     prompt <- shiny::div(
#       # insert the feedback / "Your response was correct"
#       shiny::h4(psychTestR::i18n(key, html = T, sub = list(feedback = feedback)),
#                 style = "text-align: justify; margin-left:20%;margin-right:20%; margin-bottom:20px; display:block"),
#
#       # include video
#       shiny::tags$video(
#         id = video_id,
#         src = video_url,
#         type = "video/mp4",
#         width = "560px",
#         height = "240px",
#         controls = "controls"
#         # Removed autoplay and insert a delay
#       ),
#       shiny::tags$script(
#         shiny::HTML(
#           sprintf("setTimeout(function() {
#                     var videoElement = document.getElementById('%s');
#                     videoElement.play();
#                   }, 500);", video_id) #include .500 sec of delay before playing, so that video and audio have time to load and play synchron
#         ) # see also get_audio_ui if you want to change this value
#       )
#     )
#   }
#
#   if (page_no == 5) {
#     page <- ask_repeat(prompt)
#   }
#   else{
#     browser()
#
#     seed <- get_reproducible_seed() # this specifies how long the same items are used in training
#     set.seed(seed)
#     print(seed)
#
#     if (audio_dir == "https://media.gold-msi.org/test_materials/MSAT") {
#     # use this version instead for online version
#     sample_data <- MSA::MSA_itembank_training_only %>%
#       dplyr::filter(version == "short") %>%
#       dplyr::filter(online == "yes") %>%
#       dplyr::select(-version, -online) %>%
#       dplyr::slice_sample(n = 6) %>% as.list()
#     }
#
#     # for the offline version --> more practice questions
#     if(stringr::str_detect(audio_dir, "http://127.0.0.1") == TRUE){ # check if local host is used, if yes, override with local repo.
#       sample_data <- MSA::MSA_itembank_training_only %>%
#         dplyr::filter(version == "short") %>%
#         dplyr::select(-version, -online) %>%
#         dplyr::slice_sample(n = 6) %>% as.list()
#     }
#
#     page <- MSA_item(
#       # label = sprintf("training%s", page_no),
#       label = sample_data$label[[page_no]],
#       correct_answer = sample_data$sample_audios_answers[page_no],
#       prompt = prompt,
#       audio_dir = audio_dir,
#       audio_file = sample_data$sample_audios[[page_no]],
#       adaptive = FALSE,
#       # adaptive = TRUE,
#       save_answer = FALSE,
#       instruction_page = FALSE
#     )
#   }
#   # browser()
#   page
# }


get_practice_page <- function(page_no, feedback, audio_dir) {
  # browser()
  key <- sprintf("PRACTICE%d", page_no)
  if (page_no == 5) {
    # browser()
    key <- "TRANSITION"

    # create the text each trial:
    # insert the feedback / "Your response was correct"
    # this prompt only for transition (without the video)
    prompt <- shiny::div(
      psychTestR::i18n(key, html = T, sub = list(feedback = feedback)),
      style = "text-align: justify; margin-left:20%;margin-right:20%; margin-bottom:20px; display:block")
  }
  if (page_no != 5) {
    # include video as well
    video_url <- paste0(audio_dir, "/MSA_visual_480p.mp4")
    video_id <- "MSA_visual_480p"
    # create the text each trial
    prompt <- shiny::div(
      # insert the feedback / "Your response was correct"
      shiny::h4(psychTestR::i18n(key, html = T, sub = list(feedback = feedback)),
                style = "text-align: justify; margin-left:20%;margin-right:20%; margin-bottom:20px; display:block"),

      # include video
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
                  }, 500);", video_id) #include .500 sec of delay before playing, so that video and audio have time to load and play synchron
        ) # see also get_audio_ui if you want to change this value
      )
    )
  }

  if (page_no == 5) {
    page <- ask_repeat(prompt)
  }
  else{
    # browser()

    seed <- get_reproducible_seed() # this specifies how long the same items are used in training
    set.seed(seed)
    print(seed)


    if (audio_dir == "https://media.gold-msi.org/test_materials/MSAT") {
      # use this version instead for online version
      sample_data <- MSA::MSA_itembank_training_only %>%
        dplyr::filter(version == "short") %>%
        dplyr::filter(online == "yes") %>%
        dplyr::select(-version, -online) %>%
        dplyr::slice_sample(n = 6) %>% as.list()
    }

    # This version is working only localy, as the sample audios are not on the server yet
    # for the offline version --> more practice questions
    if(stringr::str_detect(audio_dir, "http://127.0.0.1") == TRUE){ # check if local host is used, if yes, override with local repo.
      sample_data <- MSA::MSA_itembank_training_only %>%
        dplyr::filter(version == "short") %>%
        dplyr::select(-version, -online) %>%
        dplyr::slice_sample(n = 6) %>% as.list()
    }



    page <- MSA_item(
      # label = sprintf("training%s", page_no),
      label = sample_data$label[[page_no]],
      correct_answer = sample_data$sample_audios_answers[page_no],
      prompt = prompt,
      audio_dir = audio_dir,
      audio_file = sample_data$sample_audios[[page_no]],
      adaptive = FALSE,
      # adaptive = TRUE,
      save_answer = FALSE,
      instruction_page = FALSE
    )
  }
  # browser()
  # #test shit
  page
}


practice <- function(audio_dir) {
  lapply(1:5, make_practice_page, audio_dir) %>% unlist()
}



###### Version Long --------------------------------------------------------------

# # example stimuli for practice/training session
# sample_data_long <- list(
#   label =
#     c(
#       "bass_long_wit_level(-10)_complexity(3)_time(18)_songnr(80)",
#       "lead_long_wit_level(0)_complexity(3)_time(122)_songnr(20)",
#       "bass_long_wit_level(0)_complexity(6)_time(180)_songnr(37)",
#       "lead_long_wit_level(0)_complexity(3)_time(52)_songnr(93)",
#       "lead_long_wit_level(-10)_complexity(6)_time(102)_songnr(49)",
#       "bass_long_wit_level(0)_complexity(6)_time(138)_songnr(49)"
#     ),
#   sample_audios =
#     c(
#       "bass_long_wit_level(-10)_complexity(3)_time(18)_songnr(80).wav",
#       "lead_long_wit_level(0)_complexity(3)_time(122)_songnr(20).wav",
#       "bass_long_wit_level(0)_complexity(6)_time(180)_songnr(37).wav",
#       "lead_long_wit_level(0)_complexity(3)_time(52)_songnr(93).wav",
#       "lead_long_wit_level(-10)_complexity(6)_time(102)_songnr(49).wav",
#       "bass_long_wit_level(0)_complexity(6)_time(138)_songnr(49).wav"
#     ),
#   sample_audios_answers = c(1, 1, 1, 1, 1, 1)
# ) %>% as_tibble() %>% dplyr::slice_sample(n = 6) %>%
#   as.list() # randomize list
#
# # my_package_function <- function() {
#   data("MSA_itembank_training_only", package = "MSA", envir = environment())
#   # Use MSA_itembank_training_only here
# }


get_reproducible_seed <- function() {
  current_time <- Sys.time()
  time_in_seconds <- as.numeric(current_time)

  # Calculate minutes since the beginning of the current 1-minute blocks
  minutes_in_block <- floor(time_in_seconds / 60) %/% 1 # set this to the value you like | values equals the minutes after which a new testing set is drawn

  seed <- as.integer(minutes_in_block)
  return(seed)
}


get_practice_page_long <- function(page_no, feedback, audio_dir) {
  # browser()
  key <- sprintf("PRACTICE%d", page_no)
  if (page_no == 5) {
    # browser()
    key <- "TRANSITION"

    # create the text each trial:
    # insert the feedback / "Your response was correct"
    # this prompt only for transition (without the video)
    prompt <- shiny::div(
        psychTestR::i18n(key, html = T, sub = list(feedback = feedback)),
        style = "text-align: justify; margin-left:20%;margin-right:20%; margin-bottom:20px; display:block")
}
  if (page_no != 5) {
  # include video as well
  video_url <- paste0(audio_dir, "/MSA_long_visual_480p.mp4")
  video_id <- "MSA_long_visual_480p"
  # create the text each trial
  prompt <- shiny::div(
      # insert the feedback / "Your response was correct"
      shiny::h4(psychTestR::i18n(key, html = T, sub = list(feedback = feedback)),
              style = "text-align: justify; margin-left:20%;margin-right:20%; margin-bottom:20px; display:block"),

 # include video
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
                  }, 500);", video_id) #include .500 sec of delay before playing, so that video and audio have time to load and play synchron
        ) # see also get_audio_ui if you want to change this value
      )
    )
  }

  if (page_no == 5) {
    page <- ask_repeat(prompt)
  }
else{
    # browser()

    seed <- get_reproducible_seed() # this specifies how long the same items are used in training
    set.seed(seed)
    print(seed)


    if (audio_dir == "https://media.gold-msi.org/test_materials/MSAT") {
      # use this version instead for online version
      sample_data_long <- MSA::MSA_itembank_training_only %>%
        dplyr::filter(version == "long") %>%
        dplyr::filter(online == "yes") %>%
        dplyr::select(-version, -online) %>%
        dplyr::slice_sample(n = 6) %>% as.list()
    }

    # This version is working only local, as the sample audios are not on the server yet
    # for the offline version --> more practice questions
    if(stringr::str_detect(audio_dir, "http://127.0.0.1") == TRUE){ # check if local host is used, if yes, override with local repo.
      sample_data_long <- MSA::MSA_itembank_training_only %>%
        dplyr::filter(version == "long") %>%
        dplyr::select(-version, -online) %>%
        dplyr::slice_sample(n = 6) %>% as.list()
    }



    page <- MSA_item(
      # label = sprintf("training%s", page_no),
      label = sample_data_long$label[[page_no]],
      correct_answer = sample_data_long$sample_audios_answers[page_no],
      prompt = prompt,
      audio_dir = audio_dir,
      audio_file = sample_data_long$sample_audios[[page_no]],
      adaptive = FALSE,
      # adaptive = TRUE,
      save_answer = FALSE,
      instruction_page = FALSE
    )
  }
  # browser()
  page
}


make_practice_page_long <- function(page_no, audio_dir) {
  psychTestR::reactive_page(function(answer, ...) {
    # browser()
    correct <- "INCORRECT"
    if (page_no > 1) {
        answer <- answer %>%
        dplyr::mutate(correct = dplyr::case_when(
          answer == 1 & stringr::str_sub(label, -1) == "Y" ~ TRUE,
          answer == 1 & stringr::str_sub(label, -1) == "N" ~ FALSE,
          answer == 2 & stringr::str_sub(label, -1) == "N" ~ TRUE,
          answer == 2 & stringr::str_sub(label, -1) == "Y" ~ FALSE,

          answer == 1 & stringr::str_detect(label, "wit")  ~ TRUE,
          answer == 1 & stringr::str_detect(label, "wot")   ~ FALSE,
          answer == 2 & stringr::str_detect(label, "wit")  ~ FALSE,
          answer == 2 & stringr::str_detect(label, "wot")  ~ TRUE,
                    TRUE ~ FALSE  # Default case
        ))

      answer = answer[1,]} # Fix this: gets "NA" otherswise...
    if (page_no > 1 && answer$correct) {
      correct <- "CORRECT"}
    feedback <- psychTestR::i18n(correct)
    get_practice_page_long(page_no, feedback, audio_dir)
    # browser()
  })
}


practice_long <- function(audio_dir) {
  lapply(1:5, make_practice_page_long, audio_dir) %>% unlist()
}
