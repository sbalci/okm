library(stringdist)
library(logging)

levenshtein_ratio <- function(a, b) {
  lv_dist = stringdist(a, b, method = "lv")
  lv_ratio = lv_dist/(max(stri_length(a), stri_length(b)))
  return(lv_ratio)
}

check_metadata <- function (field) {
  if(!is.null(field)) {
    return (ifelse(is.na(field), '', field))
  } else {
    return ('')
  }
}


get_stopwords <- function(lang, testing) {
  stops <- tryCatch({
    stops <- stopwords(lang)
  }, error = function(err){
    return(unlist(""))
  })

  stops <- tryCatch({
      # trycatch switch when in test mode
      if (!isTRUE(testing)) {
          add_stop_path <- paste0("../resources/", lang, ".stop")
          additional_stops <- scan(add_stop_path, what="", sep="\n")
          stops = c(stops, additional_stops)
        } else if (dir.exists("./resources")) {
          add_stop_path <- paste0("./resources/", lang, ".stop")
          additional_stops <- scan(add_stop_path, what="", sep="\n")
          stops = c(stops, additional_stops)
        } else {
          add_stop_path <- paste0("../../resources/", lang, ".stop")
          additional_stops <- scan(add_stop_path, what="", sep="\n")
          stops = c(stops, additional_stops)
          return(stops)
        }}, error = function(err) {
        return(stops)
      })
  return(stops)
}

conditional_lowercase <- function(text, lang) {
  if (lang == 'german') {
    return(text)
  } else {
    return(tolower(text))
  }
}

setup_logging <- function(loglevel) {
  # checks if HEADSTART_LOGFILE is defined,
  # if not logs to console only
  if (Sys.getenv("HEADSTART_LOGFILE") == ""){
    getLogger(loglevel)
    removeHandler('basic.stdout')
    addHandler(writeToConsole)
  } else {
    if (!file.exists(Sys.getenv("HEADSTART_LOGFILE"))) {
      file.create(Sys.getenv("HEADSTART_LOGFILE"))
    }
    getLogger(loglevel)
    removeHandler('basic.stdout')
    addHandler(writeToFile, file=Sys.getenv("HEADSTART_LOGFILE"))
  }
}


get_service_lang <- function(lang_id, valid_langs, service) {
  if (lang_id == 'all'){
    LANGUAGE <- 'english'
  } else if (!is.null(valid_langs$lang_id)){
    LANGUAGE <- valid_langs$lang_id
  } else {
    LANGUAGE <- 'english'
  }
  if (service == 'linkedcat' || service == 'linkedcat_authorview' || service == "linkedcat_browseview") {
      lang_id <- 'ger'
      LANGUAGE <- 'german'
    }
  return (list(lang_id = lang_id, name = LANGUAGE))
}


detect_error <- function(failed) {
  output <- list()
  reason <- list()
  # first identify criteria
  if (length(unlist(strsplit(failed$query, " "))) < 4) {
    reason <- c(reason, 'typo', 'too specific')
  } else {
    reason <- c(reason, 'query length', 'too specific')
  }
  if (!is.null(failed$params$to) &&
      !is.null(failed$params$from) &&
      difftime(failed$params$to, failed$params$from) <= 60) {
    reason <- c(reason, 'timeframe too short')
  }
  # then return them as json list
  output$reason <- reason
  output$status <- 'error'
  return(toJSON(output, auto_unbox = TRUE))
}
