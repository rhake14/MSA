media_js <- list(
  media_not_played = "var media_played = false;",
  media_played = "media_played = true;",
  play_media = "document.getElementById('media').play();",
  show_media   = paste0("if (!media_played) ",
                        "{document.getElementById('media')",
                        ".style.visibility='inherit'};"),
  hide_media   = paste0("if (media_played) ",
                          "{document.getElementById('media')",
                          ".style.visibility='hidden'};"),
  show_media_btn = paste0("if (!media_played) ",
                          "{document.getElementById('btn_play_media')",
                          ".style.visibility='inherit'};"),
  hide_media_btn = paste0("document.getElementById('btn_play_media')",
                          ".style.visibility='hidden';"),
  show_responses = "document.getElementById('response_ui').style.visibility = 'inherit';"
)

#media_mobile_play_button <- shiny::tags$button(
#  shiny::tags$strong(psychTestR::i18n("CLICK_HERE_TO_PLAY")),
#  id = "btn_play_media",
#  style = "visibility: visible;height: 50px",
#  onclick = media_js$play_media
#)

media_mobile_play_button <- shiny::tags$p(
  shiny::tags$button(shiny::tags$span("\u25B6"),
                     type = "button",
                     id = "btn_play_media",
                     style = "visibility: hidden",
                     onclick = media_js$play_media)
)
# print(url)
# browser()

# get_audio_ui <- function(url,
#                          type = tools::file_ext(url),
#                          autoplay = TRUE,
#                          width = 0,
#                          wait = TRUE,
#                          loop = FALSE) {
#   print(url)
#   stopifnot(purrr::is_scalar_character(url),
#             purrr::is_scalar_character(type),
#             purrr::is_scalar_logical(wait),
#             purrr::is_scalar_logical(loop))
#   src    <- shiny::tags$source(src = url, type = paste0("audio/", type))
#   script <- shiny::tags$script(shiny::HTML(media_js$media_not_played))
#   audio  <- shiny::tags$audio(
#     script,
#     src,
#     id = "media",
#     preload = "auto",
#     autoplay = if(autoplay) "autoplay",
#     width = width,
#     loop = if (loop) "loop",
#     oncanplaythrough = media_js$show_media_btn,
#     onplay = paste0(media_js$media_played, media_js$hide_media_btn),
#     #onended = if (wait) paste0(media_js$show_responses, media_js$hide_media) else "null",
#     onended = if (wait) media_js$show_responses else "null"
#   )
#   shiny::tags$div(audio, media_mobile_play_button)
# }


# include a delay of 2 sec
get_audio_ui <- function(url,
                         type = tools::file_ext(url),
                         autoplay = TRUE,
                         width = 0,
                         wait = TRUE,
                         loop = FALSE) {
  print(url)
  stopifnot(purrr::is_scalar_character(url),
            purrr::is_scalar_character(type),
            purrr::is_scalar_logical(wait),
            purrr::is_scalar_logical(loop))
  src    <- shiny::tags$source(src = url, type = paste0("audio/", type))
  script <- shiny::tags$script(shiny::HTML(media_js$media_not_played))
  audio  <- shiny::tags$audio(
    script,
    src,
    id = "media",
    preload = "auto",
    # Removed autoplay attribute
    width = width,
    loop = if (loop) "loop",
    oncanplaythrough = media_js$show_media_btn,
    onplay = paste0(media_js$media_played, media_js$hide_media_btn),
    onended = if (wait) media_js$show_responses else "null"
  )
  delay_script <- shiny::tags$script(shiny::HTML(
    "setTimeout(function() {
      var mediaElement = document.getElementById('media');
      mediaElement.play();
    }, 500);" #include 1 sec of delay before playing, so that video and audio have time to load and play synchron
  ))
  shiny::tags$div(audio, delay_script, media_mobile_play_button)
}










get_audio_element <- function(url,
                              type = tools::file_ext(url),
                              wait = F,
                              autoplay = FALSE,
                              width = 200,
                              height = 50,
                              id = "media") {
  # print(url)
  stopifnot(purrr::is_scalar_character(url),
            purrr::is_scalar_character(type)
            )
  src    <- shiny::tags$source(src = url, type = paste0("audio/", type))
  script <- shiny::tags$script(shiny::HTML(media_js$media_not_played))
  audio  <- shiny::tags$audio(
    src,
    script,
    id = id,
    preload = "auto",
    controls = "controls",
    controlslist = "nodownload noremoteplayback",
    autoplay = if(autoplay) "autoplay",
    width = width,
    height = height,
    onplay = paste0(media_js$media_played, media_js$hide_media),
    onended = if (wait) paste0(media_js$show_responses, media_js$hide_media) else "null"
  )
  audio
}

audio_NAFC_page_flex <- function(label,
                                 prompt,
                                 choices,
                                 audio_url,
                                 correct_answer,
                                 adaptive = adaptive,
                                 long_version = long_version,
                                 save_answer = TRUE,
                                 get_answer = NULL,
                                 on_complete = NULL,
                                 admin_ui = NULL) {
  stopifnot(purrr::is_scalar_character(label))
  audio_ui <-
    get_audio_ui(audio_url,
                 wait = T,
                 loop = F,
                 width = 200)
  style <- NULL
  ui <- shiny::div(
    tagify(prompt),
    audio_ui,
    psychTestR::make_ui_NAFC(
      choices,
      # labels = choices,
      # labels = c("Yes","No"),
      # browser(),
      labels = c(psychTestR::i18n("BUTTON_YES"),psychTestR::i18n("BUTTON_NO")),
      hide = TRUE,
      arrange_vertically = FALSE,
      id = "response_ui"
    )
  )

  # new adaptive stuff
  if (adaptive){

    if(is.null(get_answer)){

      get_answer <- function(input, ...) {

        as.numeric(gsub("answer", "", input$last_btn_pressed))

      }
      validate <- function(answer, ...) !is.null(answer)
    }
  }
  else {

  get_answer <- function(state, input, ...) {

    i_row <- psychTestR::get_local(key = "i_row", state = state)

    if(!is.null(i_row)){
      psychTestR::set_local(key = "i_row", value = i_row + 1L , state = state)
      # messagef("Set item number: %d", i_row + 1L)
    }

    answer <- as.numeric(gsub("answer", "", input$last_btn_pressed))
    # browser()
    correct <- MSA::MSA_item_bank[MSA::MSA_item_bank$item_number == label,]$correct == answer
    # browser()

    # use this, because "item_number" works only for the short version, not for the long version
    if (grepl("long", label)) {correct <- MSA::MSA_item_bank[MSA::MSA_item_bank$long_item_number == label,]$correct == answer}

    if(length(correct) == 0) {correct <- MSA::MSA_itembank_training_only[MSA::MSA_itembank_training_only$label == label,]$sample_audios_answers == answer}

    tibble(answer = answer,
           label = label,
           correct = correct)
  }
   validate <- function(answer, ...) !is.null(answer)
}


  psychTestR::page(ui = ui,
                   label = label,
                   get_answer = get_answer,
                   save_answer = save_answer,
                   validate = validate,
                   on_complete = on_complete,
                   final = FALSE,
                   admin_ui = admin_ui)
}


MSA_item <- function(label = "",
                     audio_file,
                     correct_answer,
                     prompt = "",
                     audio_dir = "",
                     adaptive = adaptive,
                     long_version = long_version,
                     save_answer = TRUE,
                     on_complete = NULL,
                     get_answer = NULL,
                     instruction_page = FALSE
){
  page_prompt <- shiny::div(prompt)
  # printf("MSA called for item: %s", label) # print what item is called into the item pool
  choices <- c("1", "2")
  # choices <- c("Yes", "No")
  audio_url <- file.path(audio_dir, audio_file)
  audio_NAFC_page_flex(label = label,
                       prompt = page_prompt,
                       audio_url = audio_url,
                       choices = choices,
                       correct_answer = correct_answer,
                       save_answer = save_answer,
                       get_answer = get_answer,
                       on_complete = on_complete,
                       long_version = long_version,
                       adaptive = adaptive
                       )
}

