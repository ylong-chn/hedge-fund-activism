#' This is the file containing functions to download and extract information from schedule 13D
#' The code is adapted from https://github.com/volkovacodes/Block_Codes
#' paper referenced: "Is Blockholder Diversity Detrimental?" by Miriam Schwartz-Ziv and Ekaterina Volkova (2020) 
#' avaliable as SSRN: https://papers.ssrn.com/sol3/papers.cfm?abstract_id=362193
library(data.table)
library(tidyverse)
library(Hmisc)
library(httr)
library(zoo)
library(lubridate)
library(here)
library(DBI)
library(RSQLite)
library(XML)
library(RCurl)

#' The data is downloaded and information is extracted in the following way:
#' step 1: create meta file containing information of the 13D forms that came out during the quarters specified 
#' step 2: download the forms to temporary directory and put them into database
#' step 3: extract company/fund information, specifically date and CUSIP, mainly using regex
#' step 4: match CUSIP with ticker symbol using SEC fails-to-deliver data (https://www.sec.gov/data/foiadocsfailsdatahtm)
#' step 5: get stock price for the tickers using `quantmod` package

#### Preparatory 
#' Get start date for desired quarters
#' @param start_QTR starting quarter
#' @param end_QTR end quarter
get_dates <- function(start_QTR, end_QTR) {
  
  end_QTR <- end_QTR %>%
    as.yearqtr("%YQ%q") %>%
    as.Date()
  
  all_dates <- start_QTR %>%
    as.yearqtr("%YQ%q") %>%
    as.Date()
  
  while (all_dates[length(all_dates)] < ymd(end_QTR)) {
    all_dates <- c(all_dates, all_dates[length(all_dates)] %m+% months(3))
  }
  
  return(all_dates)
}

#### STEP 1
#' Construct SEC master file 
#' First download a metafile that contains information about all available forms during the quarter
#' Then filter the schedule 13D's and save the information in a table
#' @param date start date of target quarter in 'yyyy-mm-dd' format
qtr.master.file <- function(date) {
  
  # create master txt file
  # this contains all available forms during the target quarter
  master.link <- paste0("https://www.sec.gov/Archives/edgar/full-index/", year(date), "/QTR", quarter(date), "/master.idx")
  print(sprintf("Downloading master file for quarter %d of year %s...", quarter(date), year(date)))
  
  download.file(master.link, paste0(out_dir, "tmp.txt"),
                headers = c("User-Agent" = "Yang Long longyang.chn@gmail.com")
  )
  
  master <- paste0(out_dir, "tmp.txt") %>%
    readLines() %>%
    gsub("#", "", .) %>%
    paste0(collapse = "\n") %>%
    fread(sep = "|", skip = 11) %>%
    `colnames<-`(c("cik", "name", "type", "date", "link")) %>%
    # only care about initial 13D for now 
    filter(type == "SC 13D") %>%
    # filter(grepl("SC 13(D|G)", type)) %>%
    mutate(link = paste0("https://www.sec.gov/Archives/", link)) %>%
    mutate(file = gsub(".*/", "", link))
  
  closeAllConnections()
  file.remove(paste0(out_dir, "tmp.txt"))
  return(master)
}

#### STEP 2
#' Download file into temporary directory
#' @param master table containing meta information
#' @param delay set time intervals between downloads
dwnld.files <- function(master, delay = T) {
  
  dir.create(paste0(out_dir, "temp"))
  master <- master[!duplicated(file)]
  
  for (j in 1:length(master$file)) {
    if (delay == T) Sys.sleep(0.13)
    file_name <- paste0(out_dir, "temp/", master$file[j])
    
    download.file(master$link[j], file_name,
                  quiet = F,
                  headers = c("User-Agent" = "Yang Long longyang.chn@gmail.com")
    )
  }
}

#' Put downloaded files into database 
#' @param dbname name of the database to put the info into
put.files.in.sql <- function(dbname) {
  together <- function(x) {
    return(paste(x, collapse = "\n"))
  }
  
  con <- dbConnect(SQLite(), dbname = dbname)
  dbSendQuery(conn = con, "CREATE TABLE compsubm (FILENAME TEXT, COMLSUBFILE TEXT)")
  
  path <- paste0(out_dir, "temp/")
  files <- list.files(path)
  n <- length(files)
  step <- 500
  for (i in 1:(n %/% step + 1)) {
    start <- 1 + (i - 1) * step
    end <- i * (step)
    ind <- start:min(end, n)
    
    objects <- lapply(paste0(path, files[ind]), readLines)
    clean <- lapply(objects, together)
    data <- NULL
    data$FILENAME <- files[ind]
    data <- as.data.frame(data)
    data$COMLSUBFILE <- unlist(clean)
    dbWriteTable(conn = con, name = "compsubm", data, append = T)
  }
  dbDisconnect(con)
  
  # remove temp files
  unlink(paste0(out_dir, "temp"), recursive = T)
}

#### STEP 3
#' An html parser
#' @param file input html formatted file
html2txt <- function(file) {
  require(XML)
  xpathApply(htmlParse(file, encoding = "UTF-8"), "//body", xmlValue)[[1]]
}

#' Extract entire txt file
#' @param webpage raw form 
get_body <- function(webpage) {
  require(Hmisc)
  require(stringr)
  
  webpage <- unlist(strsplit(webpage, "\n"))
  file.name <- gsub("<FILENAME>", "", grep("<FILENAME>.*$", webpage, perl = TRUE, value = TRUE))
  start.line <- grep("<DOCUMENT>.*$", webpage, perl = TRUE)
  end.line <- grep("</DOCUMENT>.*$", webpage, perl = TRUE)
  
  if (length(start.line) * length(end.line) == 0) {
    return(NA)
  }
  
  if (length(file.name) == 0) {
    return(paste0(webpage[start.line:end.line], collapse = "\n"))
  }
  
  file.ext <- tolower(gsub(".*\\.(.*?)$", "\\1", file.name[1]))
  
  start.line <- start.line[1]
  end.line <- end.line[1]
  
  if (file.ext %in% c("htm", "xls", "xlsx", "js", "css", "paper", "xsd")) {
    temp <- webpage[start.line:end.line]
    pdf.start <- grep("^<TEXT>", temp, perl = TRUE) + 1
    pdf.end <- grep("^</TEXT>", temp, perl = TRUE) - 1
    res <- try(text <- html2txt(temp[pdf.start:pdf.end]), silent = T)
    if (class(res) == "try-error") text <- temp[pdf.start:pdf.end]
  }
  
  if (file.ext == "txt") {
    text <- webpage[start.line:end.line]
    text <- paste0(text, collapse = "\n")
  }
  return(text)
}

#' Extract block containing subject information
#' @param webpage raw form 
get_sbj <- function(webpage) {
  webpage <- unlist(strsplit(webpage, "\n"))
  sbj.line <- grep("SUBJECT COMPANY:", webpage)
  fil.line <- grep("FILED BY:", webpage)
  start.line <- grep("<DOCUMENT>.*$", webpage, perl = TRUE)
  if (length(sbj.line) * length(fil.line) * length(start.line) == 0) {
    return(NA)
  }
  
  sbj.line <- tail(sbj.line, 1)
  start.line <- start.line[1]
  if (sbj.line < fil.line) {
    sbj_info <- webpage[sbj.line:fil.line]
  }
  
  if (fil.line < sbj.line) {
    sbj_info <- webpage[sbj.line:start.line]
  }
  
  
  sbj_info <- paste0(sbj_info, collapse = "\n")
  return(sbj_info)
}

#' Extract block containing filer information 
#' @param webpage raw form 
get_fil <- function(webpage) {
  webpage <- unlist(strsplit(webpage, "\n"))
  sbj.line <- grep("SUBJECT COMPANY:", webpage)
  fil.line <- grep("FILED BY:", webpage)
  start.line <- grep("<DOCUMENT>.*$", webpage, perl = TRUE)
  if (length(sbj.line) * length(fil.line) * length(start.line) == 0) {
    return(NA)
  }
  
  sbj.line <- tail(sbj.line, 1)
  start.line <- start.line[1]
  if (sbj.line < fil.line) {
    sbj_info <- webpage[sbj.line:fil.line]
    fil_info <- webpage[fil.line:start.line]
  }
  
  if (fil.line < sbj.line) {
    sbj_info <- webpage[sbj.line:start.line]
    fil_info <- webpage[fil.line:sbj.line]
  }
  
  sbj_info <- paste0(sbj_info, collapse = "\n")
  fil_info <- paste0(fil_info, collapse = "\n")
  return(fil_info)
}

#' Put extracted information into a new database
#' @param name_out the new database to put extract information
#' @param master table containing meta information
#' @param con database containing raw information 
put.into.db <- function(name_out, master, con) {

  con_out <- dbConnect(SQLite(), name_out)
  dbSendQuery(
    conn = con_out,
    "CREATE TABLE filings
              (FILENAME TEXT, FILING TEXT, SBJ TEXT, FIL TEXT, LINK TEXT,
              DATE TEXT, TYPE TEXT)"
  )
  
  files <- dbGetQuery(con, "SELECT FILENAME FROM compsubm")
  N <- length(files$FILENAME)
  step <- 10000
  
  for (i in 1:(N %/% step + 1))
  {
    print(i)
    print(Sys.time())
    ### read step amount of observations
    end <- (i - 1) * step
    start <- min(step, N - end)
    line <- paste0("SELECT * FROM compsubm LIMIT ", start, " OFFSET ", end)
    x <- dbGetQuery(con, line)
    
    ### create a table with clean SC13, subject and files info
    body_text <- lapply(x$COMLSUBFILE, get_body)
    sbj_text <- lapply(x$COMLSUBFILE, get_sbj)
    fil_text <- lapply(x$COMLSUBFILE, get_fil)
    
    df <- NULL
    df$FILENAME <- as.character(x$FILENAME)
    df <- as.data.frame(df)
    body_text <- sapply(body_text, function(x) paste0(x, collapse = " "))
    df$FILING <- body_text
    df$SBJ <- unlist(sbj_text)
    df$FIL <- unlist(fil_text)
    ### add link to a filing webpage
    df$LINK <- master$address[match(df$FILENAME, master$file)]
    df$DATE <- master$date[match(df$FILENAME, master$file)]
    df$TYPE <- master$type[match(df$FILENAME, master$file)]
    dbWriteTable(con_out, name = "filings", df, append = T)
  }
  
  dbDisconnect(con_out)
  return(1)
}

#' Further extract more detailed information from blocks
#' @param FILENAME name of the file containing 13D form 
#' @param info blocks to choose from (SBJ or FIL)
sec_header <- function(FILENAME, info) {
  require(stringr)
  df <- NULL
  df <- data.frame(FILENAME = FILENAME)
  df$CNAME <- str_extract(info, "(?<=COMPANY CONFORMED NAME:\t\t\t).*(?=\n)")
  df$CIK <- str_extract(info, "(?<=INDEX KEY:\t\t\t).*(?=\n)")
  df$SIC <- str_extract(info, "(?<=STANDARD INDUSTRIAL CLASSIFICATION:\t).*(?=\n)")
  df$IRS <- str_extract(info, "(?<=IRS NUMBER:\t\t\t\t).*(?=\n)")
  df$INC_STATE <- str_extract(info, "(?<=STATE OF INCORPORATION:\t\t\t).*(?=\n)")
  df$FYEAR_END <- str_extract(info, "(?<=FISCAL YEAR END:\t\t\t).*(?=\n)")
  
  ### get business address for the companies that have it
  ### I specify info[business] to distinguish between mail and business address
  business <- grepl("BUSINESS ADDRESS:", info)
  df$business_address_street1[business] <- str_extract(info[business], "(?<=STREET 1:\t\t).*(?=\n)")
  df$business_address_street2[business] <- str_extract(info[business], "(?<=STREET 2:\t\t).*(?=\n)")
  df$business_address_city[business] <- str_extract(info[business], "(?<=CITY:\t\t\t).*(?=\n)")
  df$business_address_state[business] <- str_extract(info[business], "(?<=STATE:\t\t\t).*(?=\n)")
  df$business_address_zip[business] <- str_extract(info[business], "(?<=ZIP:\t\t\t).*(?=\n)")
  df$business_address_phone[business] <- str_extract(info[business], "(?<=BUSINESS PHONE:\t\t).*(?=\n)")
  df$FILENAME <- NULL
  return(df)
}

#' Extract CUSIP from filing
#' @param EFiling the 'body' part retrieved previously
extract_CUSIP <- function(EFiling) {
  require(stringr)
  ## get 6 lines before and 4 lines after line CUSIP
  pat_1 <- "((\\n.*){6})CUSIP.*((\\n.*){4})"
  get_block <- str_extract(EFiling, pat_1)
  
  # set pattern to extract CUSIP
  pat_2 <- "(?=\\d.*\\d)[a-zA-Z0-9]{9}|\\d\\w{6} \\w\\w \\w|\\d\\w{5} \\w\\w \\w|
  [a-zA-Z0-9]{7}\\r|\\d\\w{5} \\w\\w\\w|(?=#.*\\d)[a-zA-Z0-9]{9}|(?=\\w\\d.*)[a-zA-Z0-9]{9}|
  \\d\\w{5}-\\w\\w-\\w|\\d\\w{5}-\\w\\w\\w|\\d\\w{6}|\\d\\w{5}-\\w{2}-\\w|\\d\\w{5}\\n.*\\n.*|
  \\d\\w{2} \\d\\w{2} \\d\\w{2}|\\d\\w{2} \\w{3} \\d{2} \\d|\\d{6} \\d{2} \\d|
  \\d\\w{4} \\w{1} \\w{2} \\w|\\w{6} \\d{2} \\d{1}|\\d{3} \\d{3} \\d{3}|\\d{6} \\d{2} \\d{1}|
  \\w{3} \\w{3} \\d{2} \\d{1}|\\w{5} \\w{1} \\d{2} \\d{1}|\\d{6} \\d{1} \\d{2}|
  \\d{3} \\d{3} \\d{1} \\d{2}|\\d\\w{2}\\n.*\\d\\w{2}|\\d{6} \\d{2}\\n.*|\\d{5} \\d{2} \\d{1}|
  \\d{5} \\w{1} \\w{2} \\w{1}|\\d\\w{5}|\\d\\w{2}-\\w{3}-\\w{3}"
  
  
  # Extract CUSIP from within the blocks extracted
  CUSIP <- str_extract(get_block, pat_2)
  return(CUSIP)
}

#' remove duplicates from CIK-CUSIP map
#' @param CUSIP CUSIP obtained from previous function
CUSIP_table <- function(CUSIP) {
  require(data.table)
  CUSIP_df <- data.table(CUSIP)
  CUSIP_df[, quarter := paste0(year(dates[i]), quarter(dates[i]))]
  CUSIP_df[, CUSIP := gsub("\\s", "", CUSIP)]
  CUSIP_df[, CUSIP := gsub("-", "", CUSIP)]
  CUSIP_df[, CUSIP := toupper(CUSIP)]
  CUSIP_df[, CUSIP6 := substr(CUSIP, 1, 6)]
  # CUSIP_df[, CUSIP := substr(CUSIP, 1, 8)]
  return(CUSIP_df)
}
