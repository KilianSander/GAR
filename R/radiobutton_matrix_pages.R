#' N-Alternative Choice Matrix Pages
#'
#' The \code{*_NAC_matrix_page} functions create a radio button matrix page.
#' They can be thought of as extensions of the
#' \code{\link[psychTestR]{NAFC_page}()} as they not only display one question
#' or item, but a list of questions or items to be answered on the same
#' response scale.
#' @name radiobutton_matrix_pages
NULL

#' @rdname radiobutton_matrix_pages
#' @export
NAC_matrix_page <- function(label,
                            polarity = c("unipolar", "bipolar"),
                            items,
                            choices,
                            instruction = NULL,
                            labels = NULL,
                            anchors = FALSE,
                            header = c("double", "simple_str", "simple_num"),
                            header_style = NULL,
                            sublabel_type = c("none", "directed", "symmetric"),
                            reduce_labels = TRUE,
                            style = default_style,
                            trigger_button_text = "Continue",
                            allow_na = FALSE,
                            failed_validation_message = "Answer missing!",
                            save_answer = TRUE,
                            hide_response_ui = FALSE,
                            # random_order = FALSE,
                            matrix_item_order = 1:length(items),
                            response_ui_id = "response_ui",
                            on_complete = NULL,
                            admin_ui = NULL) {
  stopifnot(
    is.scalar.character(label),
    is.character.vector(items),
    is.scalar.character(trigger_button_text),
    is.scalar.character(failed_validation_message),
    is.character.or.numeric(choices),
    length(choices) > 0L
  )

  sublabel_type <- match.arg(sublabel_type)

  instruction_tag <- NULL

  if(!is.null(instruction)) {
    instruction_tag <- tagify(instruction)
  }

  items <- items[matrix_item_order]

  ui <- shiny::tags$div(
    instruction_tag,
    make_ui_radiobutton_matrix(
      label,
      polarity = polarity,
      items = items,
      scale_labels = labels,
      choices,
      style = style,
      trigger_button_text = trigger_button_text,
      anchors = anchors,
      header = header,
      header_style = header_style,
      sublabel_type = sublabel_type,
      reduce_labels = reduce_labels,
      hide = hide_response_ui,
      id = response_ui_id
    )
  )

  get_answer <- function(input, ...) {
    values <-shiny::reactiveValuesToList(input)$radio_matrix
    answer <-
      purrr::map(
        values,
        function(x) {
          if(!is.null(x[[1]])) {
            as.numeric(x[[1]])
          } else NA
        }
      ) %>%
      unlist()

    names(answer) <- sprintf("%s.q%d", label, matrix_item_order)
    answer
  }

  validate <- function(answer,  ...) {
    valid  <- TRUE
    #messagef("[validate] allow_na: %s", allow_na)
    if(is.logical(allow_na)){
      if(!allow_na){
        if(any(is.na(answer))){
          valid <- failed_validation_message
        }
      }
    }
    else{
      na_count <- sum(is.na(answer))
      if(na_count > allow_na){
        valid <- failed_validation_message
      }
    }
    valid
  }

  psychTestR::page(
    ui = ui,
    label = label,
    get_answer = get_answer,
    save_answer = save_answer,
    validate = validate,
    on_complete = on_complete,
    final = FALSE,
    admin_ui = admin_ui
  )
}

#' @rdname radiobutton_matrix_pages
#' @export
audio_NAC_matrix_page <- function(label,
                                  polarity = c("unipolar", "bipolar"),
                                  items,
                                  choices,
                                  url,
                                  instruction = "",
                                  labels = NULL,
                                  anchors = FALSE,
                                  header = "double",
                                  header_style = NULL,
                                  sublabel_type = c("none", "directed", "symmetric"),
                                  style = default_style,
                                  reduce_labels = TRUE,
                                  trigger_button_text = "Continue",
                                  allow_na = FALSE,
                                  failed_validation_message = "Answer missing!",
                                  save_answer = TRUE,
                                  hide_response_ui = FALSE,
                                  # random_order = FALSE,
                                  matrix_item_order = 1:length(items),
                                  response_ui_id = "response_ui",
                                  on_complete = NULL,
                                  audio_type = tools::file_ext(url),
                                  wait = TRUE,
                                  loop = FALSE,
                                  admin_ui = NULL,
                                  btn_play_prompt = if (!show_controls) "Click here to play",
                                  show_controls = FALSE,
                                  allow_download = FALSE) {
  stopifnot(
    is.scalar.character(label),
    is.character.or.numeric(choices),
    is.scalar.character(url),
    is.scalar.logical(wait),
    is.scalar.logical(loop),
    is.scalar.logical(show_controls),
    is.scalar.logical(allow_download),
    is.scalar.logical(hide_response_ui)
  )

  audio_ui <- shiny::tags$div(shiny::tags$audio(
    shiny::tags$head(shiny::tags$script(shiny::HTML(media.js$media_not_played))),
    shiny::tags$source(src = url, type = paste0("audio/", audio_type)),
    id = "media",
    preload = "auto",
    autoplay = "autoplay",
    loop = if (loop) "loop",
    oncanplaythrough = media.js$show_media_btn,
    onplay = paste0(media.js$media_played, media.js$hide_media_btn),
    onended = if (wait) media.js$show_responses else "null",
    controls = if (show_controls) "controls",
    controlsList = if (!allow_download) "nodownload"
  ), media_mobile_play_button(btn_play_prompt))
  instruction2 <- shiny::tags$div(tagify(instruction),
                                  #shiny::span(url, style = "color:red"),
                                  audio_ui)

  NAC_matrix_page(
    label = label,
    polarity = polarity,
    items = items,
    choices = choices,
    instruction = instruction2,
    labels = labels,
    anchors = anchors,
    header = header,
    header_style = header_style,
    sublabel_type = sublabel_type,
    reduce_labels = reduce_labels,
    style = style,
    trigger_button_text = trigger_button_text,
    allow_na = allow_na,
    failed_validation_message = failed_validation_message,
    save_answer = save_answer,
    hide_response_ui = hide_response_ui,
    matrix_item_order = matrix_item_order,
    response_ui_id = response_ui_id,
    on_complete = on_complete,
    admin_ui = admin_ui
  )
}

#' @rdname radiobutton_matrix_pages
video_NAC_matrix_page <- function(label,
                                  polarity = c("unipolar", "bipolar"),
                                  items,
                                  choices,
                                  url,
                                  instruction = "",
                                  labels = NULL,
                                  anchors = FALSE,
                                  header = "double",
                                  header_style = NULL,
                                  sublabel_type = c("none", "directed", "symmetric"),
                                  style = default_style,
                                  reduce_labels = TRUE,
                                  trigger_button_text = "Continue",
                                  allow_na = FALSE,
                                  failed_validation_message = "Answer missing!",
                                  save_answer = TRUE,
                                  hide_response_ui = FALSE,
                                  # random_order = FALSE,
                                  matrix_item_order = 1:length(items),
                                  response_ui_id = "response_ui",
                                  on_complete = NULL,
                                  video_type = tools::file_ext(url),
                                  video_width = "100%",
                                  wait = TRUE,
                                  loop = FALSE,
                                  admin_ui = NULL,
                                  btn_play_prompt = if (!show_controls) "Click here to play",
                                  show_controls = FALSE,
                                  allow_download = FALSE) {
  stopifnot(
    is.scalar.character(label),
    is.character.or.numeric(choices),
    is.scalar.character(url),
    is.scalar.logical(wait),
    is.scalar.logical(loop),
    is.scalar.logical(show_controls),
    is.scalar.logical(allow_download),
    is.scalar.logical(hide_response_ui)
  )

  video_ui <-
    shiny::tags$div(
      shiny::tags$video(
        shiny::tags$head(shiny::tags$script(shiny::HTML(media.js$media_not_played))),
        shiny::tags$source(src = url, type = paste0("video/", video_type)),
        id = "media", width = video_width, preload = "auto",
        autoplay = "autoplay", style = "max-width: 500px",
        playsinline = "playsinline",
        loop = if (loop) "loop",
        oncanplaythrough = media.js$show_media_btn,
        onplay = paste0(media.js$media_played, media.js$hide_media_btn),
        onended = if (wait) media.js$show_responses else "null",
        controls = if (show_controls) "controls",
        controlsList = if (!allow_download) "nodownload",
        disablePictureInPicture = "disablePictureInPicture"
      ),
      media_mobile_play_button(btn_play_prompt)
    )

  instruction2 <- shiny::tags$div(tagify(instruction),
                                  video_ui)

  NAC_matrix_page(
    label = label,
    polarity = polarity,
    items = items,
    choices = choices,
    instruction = instruction2,
    labels = labels,
    anchors = anchors,
    header = header,
    header_style = header_style,
    sublabel_type = sublabel_type,
    reduce_labels = reduce_labels,
    style = style,
    trigger_button_text = trigger_button_text,
    allow_na = allow_na,
    failed_validation_message = failed_validation_message,
    save_answer = save_answer,
    hide_response_ui = hide_response_ui,
    matrix_item_order = matrix_item_order,
    response_ui_id = response_ui_id,
    on_complete = on_complete,
    admin_ui = admin_ui
  )
}

#' @rdname radiobutton_matrix_pages
image_NAC_matrix_page <- function(label,
                                  polarity = c("unipolar", "bipolar"),
                                  items,
                                  choices,
                                  url,
                                  img_width = "100%",
                                  instruction = NULL,
                                  labels = NULL,
                                  anchors = FALSE,
                                  header = c("double", "simple_str", "simple_num"),
                                  header_style = NULL,
                                  sublabel_type = c("none", "directed", "symmetric"),
                                  reduce_labels = TRUE,
                                  style = default_style,
                                  trigger_button_text = "Continue",
                                  allow_na = FALSE,
                                  failed_validation_message = "Answer missing!",
                                  save_answer = TRUE,
                                  hide_response_ui = FALSE,
                                  # random_order = FALSE,
                                  matrix_item_order = 1:length(items),
                                  response_ui_id = "response_ui",
                                  on_complete = NULL,
                                  admin_ui = NULL) {
  stopifnot(
    is.scalar.character(label),
    is.character.or.numeric(choices),
    is.scalar.character(url)
  )

  # PRELIMINARY!
  # img_width should automatically be adjusted to the given layout
  instruction2 <-
    shiny::tags$div(
      tagify(instruction),
      shiny::tags$img(
        width = img_width,
        src = url
      )
    )

  NAC_matrix_page(
    label = label,
    polarity = polarity,
    items = items,
    choices = choices,
    instruction = instruction2,
    labels = labels,
    anchors = anchors,
    header = header,
    header_style = header_style,
    sublabel_type = sublabel_type,
    reduce_labels = reduce_labels,
    style = style,
    trigger_button_text = trigger_button_text,
    allow_na = allow_na,
    failed_validation_message = failed_validation_message,
    save_answer = save_answer,
    hide_response_ui = hide_response_ui,
    matrix_item_order = matrix_item_order,
    response_ui_id = response_ui_id,
    on_complete = on_complete,
    admin_ui = admin_ui
  )
}
