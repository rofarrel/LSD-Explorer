## ---- include=FALSE-----------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, eval = FALSE, comment = "#>", collapse = TRUE)

## -----------------------------------------------------------------------------
#  ## install httpuv if not already
#  if (!requireNamespace("httpuv", quietly = TRUE)) {
#    install.packages("httpuv")
#  }

## -----------------------------------------------------------------------------
#  ## load rtweet
#  library(rtweet)
#  
#  ## store api keys (these are fake example values; replace with your own keys)
#  api_key <- "afYS4vbIlPAj096E60c4W1fiK"
#  api_secret_key <- "bI91kqnqFoNCrZFbsjAWHD4gJ91LQAhdCJXCj3yscfuULtNkuu"
#  
#  ## authenticate via web browser
#  token <- create_token(
#    app = "rstatsjournalismresearch",
#    consumer_key = api_key,
#    consumer_secret = api_secret_key)

## -----------------------------------------------------------------------------
#  ## view token (you should see the correct app name)
#  token

## -----------------------------------------------------------------------------
#  ## store api keys (these are fake example values; replace with your own keys)
#  api_key <- "afYS4vbIlPAj096E60c4W1fiK"
#  api_secret_key <- "bI91kqnqFoNCrZFbsjAWHD4gJ91LQAhdCJXCj3yscfuULtNkuu"
#  access_token <- "9551451262-wK2EmA942kxZYIwa5LMKZoQA4Xc2uyIiEwu2YXL"
#  access_token_secret <- "9vpiSGKg1fIPQtxc5d5ESiFlZQpfbknEN1f1m2xe5byw7"
#  
#  ## authenticate via web browser
#  token <- create_token(
#    app = "rstatsjournalismresearch",
#    consumer_key = api_key,
#    consumer_secret = api_secret_key,
#    access_token = access_token,
#    access_secret = access_token_secret)

## -----------------------------------------------------------------------------
#  ## check to see if the token is loaded
#  library(rtweet)
#  get_token()

