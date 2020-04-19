# rm(list = ls())

library(rstudioapi)

options(warn = 1)

# wd <- dirname(rstudioapi::getActiveDocumentContext()$path)
# setwd(wd) #Don't forget to set your working directory

# query <- "balci serdar" #args[2]
query <- readLines(here::here("query/query_PBPath.txt"))
service <- "pubmed"
params <- NULL
params_file <- "json/okm_params_pubmed.json"

source(here::here('R/utils.R'))
DEBUG = FALSE

if (DEBUG == TRUE) {
  setup_logging('DEBUG')
} else {
  setup_logging('INFO')
}

tslog <- getLogger('ts')

source(here::here("R/vis_layout.R"))
source(here::here('R/pubmed_use_xml.R'))

MAX_CLUSTERS = 1000

if (!is.null(params_file)) {
  params <- fromJSON(here::here(params_file))
}

if ('lang_id' %in% names(params)) {
  lang_id <- params$lang_id
} else {
  lang_id <- 'all'
}

LANGUAGE <- get_service_lang(lang_id, valid_langs, service)
ADDITIONAL_STOP_WORDS = LANGUAGE$name

#start.time <- Sys.time()
failed <- list(params = params)
tryCatch({
  input_data = get_papers(query, params)
}, error = function(err) {
  tslog$error(gsub(
    "\n",
    " ",
    paste(
      "Query failed",
      service,
      query,
      paste(params, collapse = " "),
      err,
      sep = "||"
    )
  ))
  failed$query <<- query
  failed$query_reason <<- err$message
})

#end.time <- Sys.time()
#time.taken <- end.time - start.time
#time.taken
tryCatch({
  output_json = vis_layout(
    input_data$text,
    input_data$metadata,
    service,
    max_clusters = MAX_CLUSTERS,
    lang = LANGUAGE$name,
    add_stop_words = ADDITIONAL_STOP_WORDS,
    testing = TRUE
  )
}, error = function(err) {
  tslog$error(gsub(
    "\n",
    " ",
    paste(
      "Processing failed",
      query,
      paste(params, collapse = " "),
      err,
      sep = "||"
    )
  ))
  failed$query <<- query
  failed$processing_reason <<- err$message
})

if (!exists('output_json')) {
  output_json <- detect_error(failed)
}

# print(output_json)

write(output_json,
      file = here::here("out",
                        paste0(
                          as.character(
                            Sys.time())
                            , "_output_json.json")
                        )
      )


output_df <- jsonlite::fromJSON(output_json)

write(x = query, file = here::here("out" , paste0(as.character(Sys.time(
)), "_query.txt")))

readr::write_csv(output_df, path = here::here("out" , paste0(
  as.character(Sys.time()),
  "_output_df.csv"
)))

rio::export(x = output_df,
            file = here::here("out" ,
                              paste0(
                                as.character(Sys.time()),
                                "_output_df.xlsx")),
                                format = "xlsx"
            )

readr::write_csv(input_data$metadata, path = here::here("out" , paste0(as.character(Sys.time(
)),
"_metadata.csv")))

rio::export(x = input_data$metadata,
            file = here::here("out" ,
                              paste0(
              as.character(Sys.time()),
              "_metadata.xlsx"
            )), format = "xlsx")

readr::write_csv(input_data$text, path = here::here("out" , paste0(as.character(Sys.time(
)), "_text.csv")))

rio::export(x = input_data$text,
            file = here::here("out" , paste0(as.character(Sys.time(
            )), "_text.xlsx")), format = "xlsx")

