##==========================================##
##               LOAD PACKAGES              ##
##==========================================##

library(stringi)
library(stringr)
library(dplyr)
library(purrr)
library(checkmate)
library(shiny)
library(ggplot2)
library(lemon)

get_count <- function(x){
  x %>% 
    stri_replace_all_regex(pattern = c(";", "/"),
                           replacement = ",",
                           vectorize_all = F) %>% 
    paste0("c(", ., ")") %>% 
    parse(text = .) %>% 
    eval()
}

get_cats <- function(x){
  x %>% 
    stri_replace_all_regex(pattern = c(" ,", " ;", ", ", "; ", ";", "/ ", "/"),
                           replacement = ",",
                           vectorize_all = F) %>% 
    str_split(pattern = ",") %>% 
    unlist()
}

check_input_bf <- function(Y1, Y2, b1, b2, chains, iter, warmup){
  
  mssge <- character(0)
  err <- FALSE
  wrng <- NULL
  
  Y1_check <- check_numeric(safely(get_count)(Y1)$result,
                            lower = 0, any.missing = F, all.missing = F)
  Y2_check <- check_numeric(safely(get_count)(Y2)$result,
                            lower = 0, any.missing = F, all.missing = F)
  if(any(!isTRUE(Y1_check), !isTRUE(Y2_check))){
    mssge <- c(mssge, "Please make sure that data input is in the right format.")
    err <- TRUE
  }
  
  b1_check <- check_numeric(b1,
                            lower = 0, any.missing = F, all.missing = F)
  b2_check <- check_numeric(b2,
                            lower = 0, any.missing = F, all.missing = F)
  if(any(!isTRUE(b1_check), !isTRUE(b2_check))){
    mssge <- c(mssge, "Please check your prior settings.")
    err <- TRUE
  }else{
    len_y1 <- length(get_count(Y1))
    len_y2 <- length(get_count(Y2))
    if(len_y1 != len_y2){
      mssge <- c(mssge, "Number of categories must be the same in each condition.")
      err <- TRUE
    }
  }
  
  chains_check <- check_integerish(chains,
                                   lower = 1, any.missing = F, all.missing = F)
  iter_check <- check_numeric(iter,
                              lower = 1, any.missing = F, all.missing = F)
  warmup_check <- check_numeric(warmup,
                                lower = 1, any.missing = F, all.missing = F)
  
  if(any(!isTRUE(chains_check), !isTRUE(warmup_check))){
    mssge <- c(mssge, "Please check MCMC settings.")
    err <- TRUE
  }
  
  if(!isTRUE(iter_check)){
    mssge <- c(mssge, "Please check the number of posterior samples.")
    err <- TRUE
  }else if((iter * chains) < 10000){
    wrng <- "Total No of posterior samples is small. Consider increasing."
  }
  
  mssge <- paste(mssge, collapse = "\n")
  
  out <- list(
    error = err,
    message = mssge,
    warning = wrng
  )
  
  return(out)
  
}


check_input_data_plot <- function(Y1, Y2, cats, labels){
  
  mssge <- character(0)
  err <- FALSE
  
  Y1_check <- check_numeric(safely(get_count)(Y1)$result,
                            lower = 0, any.missing = F, all.missing = F)
  Y2_check <- check_numeric(safely(get_count)(Y2)$result,
                            lower = 0, any.missing = F, all.missing = F)
  if(any(!isTRUE(Y1_check), !isTRUE(Y2_check))){
    mssge <- c(mssge, "Please make sure that data input is in the right format.")
    err <- TRUE
  }else{
    len_y1 <- length(get_count(Y1))
    len_y2 <- length(get_count(Y2))
    if(len_y1 != len_y2){
      mssge <- c(mssge, "Number of categories must be the same in each condition.")
      err <- TRUE
    }
  }
  
  if(labels){
    cats_check <- check_character(safely(get_cats)(cats)$result,
                                  any.missing = F, all.missing = F)
    if(!isTRUE(cats_check)){
      mssge <- c(mssge, "Please make sure that category labels are in the right format.")
      err <- TRUE
    }else{
      if(length(get_cats(cats)) != len_y1){
        mssge <- c(mssge, "Number of category labels is not correct.")
        err <- TRUE
      }
    }
  }
  
  mssge <- paste(mssge, collapse = "\n")
  
  out <- list(
    error = err,
    message = mssge
  )
  
  return(out)
  
}


check_input_mcmc_plot <- function(samples){
  
  mssge <- character(0)
  err <- FALSE
  
  if(is_empty(samples)){
    mssge <- "Can't plot MCMC samples. Please run analysis first."
    err <- TRUE
  }
  
  out <- list(
    error = err,
    message = mssge
  )
  
  return(out)
  
}

