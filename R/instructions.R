info_page <- function(id, style = "text-align:justify; margin-left:20%;margin-right:20%") {
  psychTestR::one_button_page(shiny::div(psychTestR::i18n(id, html = TRUE),
                                         style = style),
                              button_text = psychTestR::i18n("CONTINUE"))
}

# two audio examples for instructions
# DE:"In diesem Beispiel spielt das Zielinstrument (Hauptstimme) in der Instrumentenmixtur!"
demo_sample1 <- "voc_Item2436_set462_wit_lvl0_comp3_t350_song5.wav"
# DE:"In diesem Musikausschnitt spielt das Zielinstrument (Hauptstimme) nicht in der Mixtur."
demo_sample2 <- "voc_Item3403_set877_wot_lvl0_comp4_t62_song94.wav"


get_video_element <- function(url,
                              type = tools::file_ext(url),
                              wait = F,
                              autoplay = T, # False is original
                              width = 600,
                              height = 400,
                              id = "media") {
  # print(url)
  stopifnot(purrr::is_scalar_character(url),
            purrr::is_scalar_character(type)
  )
  src    <- shiny::tags$source(src = url, type = paste0("video/", type))
  script <- shiny::tags$script(shiny::HTML(media_js$media_not_played))
  video  <- shiny::tags$video(
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
  video
}

show_tutorial_page <- function(audio_dir){
  video_url <- "https://www.testable.org/experiment/4346/515133/stimuli/msa-tutorial-video.mp4"
  # audio_url <- file.path(audio_dir, sprintf("%s.mp3", demo_sample))
  video <- get_video_element(url = video_url, autoplay = F)
  body <- shiny::div(
    shiny::div(psychTestR::i18n("CONTINUE"),
               style = "text-align: justify; margin-left:20%;margin-right:20%;margin-bottom:20px"),
    shiny::p(video)
  )
  psychTestR::one_button_page(
    body = body,
    button_text = psychTestR::i18n("CONTINUE")
  )

}


instructions <- function(audio_dir, long_version, with_picture, with_video, take_training) {

  ### strange workaround to get the pictures included correctly
  # picture 1 has 720p
  picture <- psychTestR::i18n("INSTRUCTIONS_PIC1", html = FALSE)
  picture <- sub("\\\">.*", "", picture)
  picture <- sub(".*href=\"", "", picture)
  # picture 2 has 1080p
  picture2 <- psychTestR::i18n("INSTRUCTIONS_PIC2", html = FALSE)
  picture2 <- sub("\\\">.*", "", picture2)
  picture2 <- sub(".*href=\"", "", picture2)

  picture_long <- psychTestR::i18n("INSTRUCTIONS_PIC_LONG", html = FALSE)
  picture_long <- sub("\\\">.*", "", picture_long)
  picture_long <- sub(".*href=\"", "", picture_long)
  # picture 2 has 1080p
  picture_short <- psychTestR::i18n("INSTRUCTIONS_PIC_SHORT", html = FALSE)
  picture_short <- sub("\\\">.*", "", picture_short)
  picture_short <- sub(".*href=\"", "", picture_short)


  # prepare video import
  video1 <- psychTestR::i18n("INSTRUCTIONS_VIDEO", html = FALSE)
  video1 <- sub("\\\">.*", "", video1)
  video1 <- sub(".*href=\"", "", video1)

  # include codeblock
  c(
    psychTestR::code_block(function(state, ...) {
      psychTestR::set_local("do_intro", TRUE, state)
    }),

    if (long_version == F) {
      info_page("INSTRUCTIONS")
    }else {info_page("INSTRUCTIONS_LONG")}
    ,
    # browser(),
    # psychTestR::one_button_page(
    #   shiny::div(shiny::tags$img(src = picture),
    #              style = "text-align:center; margin-left:20%;margin-right:20%"),
    #   button_text = psychTestR::i18n("CONTINUE"),
    # ),

    ### presenting just a picture
    ### # provide in english and german is nessesarry
    if (with_picture == TRUE) {
      if (long_version == F) {
        psychTestR::one_button_page(
          shiny::div(
            shiny::tags$img(
              src = picture_short,
              height = "80%",
              width = "80%"
            ),
            style = "text-align:center;margin-left:auto;margin-right:auto"
          ),
          button_text = psychTestR::i18n("CONTINUE"),
        )
      }else {
        psychTestR::one_button_page(
          shiny::div(
            shiny::tags$img(
              src = picture_long,
              height = "80%",
              width = "80%"
            ),
            style = "text-align:center;margin-left:auto;margin-right:auto"
          ),
          button_text = psychTestR::i18n("CONTINUE"),
        )
      }

    }
    ,

    ### include a video as instructional support
    if (with_video == TRUE) {

      psychTestR::one_button_page(
        shiny::div(
          shiny::tags$video(
            src = "https://www.testable.org/experiment/4346/515133/stimuli/MSA_tutorial_video_720.mp4",
            type = "video/mp4",
            width = "900px",
            height = "600px",
            controls = "controls"
          ),
          style = "text-align:center; margin-left:auto;margin-right:auto"
        ),
        button_text = psychTestR::i18n("CONTINUE")
      )
    }
    ,
    # browser(),
    # show_tutorial_page(video1), # is not working yet, so need to fix this

    if (take_training) {
      if (long_version) {
        psychTestR::while_loop(
          test = function(state, ...)
            psychTestR::get_local("do_intro", state),
          logic = practice_long(audio_dir)
        )
      }else {
        show_sample_page(audio_dir)
        show_second_sample_page(audio_dir)
        psychTestR::while_loop(
          test = function(state, ...)
            psychTestR::get_local("do_intro", state),
          logic = practice(audio_dir)

        )
      }
    },
    # browser(),
    psychTestR::one_button_page(
      shiny::div(psychTestR::i18n("MAIN_INTRO"),
                 style = "text-align: justify; margin-left:20%; margin-right:20%; margin-bottom:20px; display:block"),
      button_text = psychTestR::i18n("CONTINUE"))
  )
}


show_sample_page <- function(audio_dir){
  audio_url <- file.path(audio_dir, sprintf(demo_sample1))
  # audio_url <- file.path(audio_dir, sprintf("%s.mp3", demo_sample))
  audio <- get_audio_element(url = audio_url, autoplay = F)
  body <- shiny::div(
    shiny::div(psychTestR::i18n("SAMPLE1a"),
               style = "text-align: justify; margin-left:20%; margin-right:20%; margin-bottom:20px; display:block"),
    shiny::p(audio)
  )
  psychTestR::one_button_page(
    body = body,
    button_text = psychTestR::i18n("CONTINUE")
  )

}

show_second_sample_page <- function(audio_dir){
  audio_url <- file.path(audio_dir, sprintf(demo_sample2))
  # audio_url <- file.path(audio_dir, sprintf("%s.mp3", demo_sample))
  audio <- get_audio_element(url = audio_url, autoplay = F)
  body <- shiny::div(
    shiny::div(psychTestR::i18n("SAMPLE1b"),
               style = "text-align: justify; margin-left:20%; margin-right:20%; margin-bottom:20px; display:block"),
    shiny::p(audio)
  )
  psychTestR::one_button_page(
    body = body,
    button_text = psychTestR::i18n("CONTINUE")
  )

}



