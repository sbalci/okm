library(logging)
library(GMD)
library(MASS)
library(vegan)
library(tm)
library(textcat)
library(proxy)
library(SnowballC)
library(jsonlite)
library(parfossil)
library(doParallel)
library(stringi)
library(stringdist)
library(plyr)
library(onehot)
registerDoParallel(3)


vlog <- getLogger('vis')

# Expects the following metadata fields:
# id, content, title, readers, published_in, year, authors, paper_abstract, subject

vis_layout <- function(text, metadata, service,
                       max_clusters=15, maxit=500,
                       mindim=2, maxdim=2,
                       lang=NULL, add_stop_words=NULL,
                       testing=FALSE, taxonomy_separator=NULL, list_size=-1,
                       vis_type='overview') {
  TESTING <<- testing # makes testing param a global variable
  start.time <- Sys.time()

  if(!exists('input_data')){
    stop("No input data found.")
  }

  tryCatch({
   if(!isTRUE(testing)) {
     source(here::here('R/preprocess.R'))
     source(here::here('R/features.R'))
     source(here::here('R/cluster.R'))
     source(here::here('R/summarize.R'))
     source(here::here('R/postprocess.R'))
   } else {
     source(here::here('R/preprocess.R'))
     source(here::here('R/features.R'))
     source(here::here('R/cluster.R'))
     source(here::here('R/summarize.R'))
     source(here::here('R/postprocess.R'))
   }
  }, error = function(err) print(err)
  )

  vlog$debug("preprocess")
  filtered <- filter_duplicates(metadata, text, list_size)
  metadata <- filtered$metadata
  text <- filtered$text

  if(vis_type=='overview'){
    metadata["lang_detected"] <- detect_language(text$content)
    stops <- get_stopwords(lang, testing)
    corpus <- create_corpus(metadata, text, lang)

    vlog$debug("get features")
    tdm_matrix <- create_tdm_matrix(corpus$stemmed)
    distance_matrix <- get_distance_matrix(tdm_matrix)
    lang_detected <- get_OHE_feature(metadata, "lang_detected")
    vlog$info(paste("Languages:",
                    paste(paste0(names(lang_detected),
                                 ":",
                                 apply(lang_detected, 2, sum)),
                          collapse = " "),
                     sep=" "))
    features <- concatenate_features(distance_matrix)
    vlog$debug("get clusters")
    clusters <- create_clusters(as.dist(features), max_clusters=max_clusters)
    layout <- get_ndms(as.dist(features), mindim=2, maxdim=2)

    vlog$debug("get cluster summaries")
    metadata = replace_keywords_if_empty(metadata, stops, service)
    named_clusters <- create_cluster_labels(clusters, metadata,
                                            service, lang,
                                            corpus$unlowered,
                                            weightingspec="ntn", top_n=3,
                                            stops=stops, taxonomy_separator)
    output <- create_overview_output(named_clusters, layout, metadata)
  } else {
    output <- create_streamgraph_output(metadata)
  }

  end.time <- Sys.time()
  time.taken <- end.time - start.time
  vlog$info(paste("Time taken:", time.taken, sep=" "))
  return(output)

}

