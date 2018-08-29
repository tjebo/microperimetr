#' get_names
#' Extract patient identifiable information from MAIA raw data
#'
#' The function will extract the data from the raw .tgz output of your MAIA.
#' It is generally not recommended to use this function in order to keep personal identifiable data as safe as possible.
#'
#' @author Tjebo
#'
#' @param folder source folder which is searched for .tgz files. Default: workdirectory
#'
#' @return Data frame
#'
#' @export

get_names <- function(folder = getwd()){

  tgz_name <-  list.files(folder)[grepl('.tgz',list.files())] #list of tgz files

  pull_out <- function(tgz_element)  {
    #tgz_element is element of tgz_name (each tgz file), and pull_out will be used with lapply to perform the whole lot on every tgz file

    tmpdir <- tempdir()
    patID <- str_extract(tgz_element, "(?i)(?<=patient)\\d+")
    utils::untar(tgz_element, exdir = tmpdir)
    tmpdir_patID <- paste0(tmpdir,'/',patID)
  ##dir of the files within the patID folder, makes object patID for the following code

  ## makes list of json data, contains patient details
    JsonData <- jsonlite::fromJSON(txt = paste0(tmpdir,"/",patID,"/",patID,".json"))

  ## list2: what you want to pull out of the .json file
    list2 <- list(patID = 'id', name = 'lastname', first_name = 'firstname', sex = 'gender', DOB = 'birthdate')

    data.frame(lapply(list2, function(z) JsonData[[1]][[z]]), stringsAsFactors = FALSE)

  }  ##end of pull_out() function to use for list of tgz files

    dplyr::bind_rows(lapply(tgz_name, pull_out))

}

get_names()
