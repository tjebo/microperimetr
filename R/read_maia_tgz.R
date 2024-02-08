#' Read MAIA files
#' @name read_maia_tgz
#' @description
#' The function will extract the anonymised data from the raw .tgz patient backup files of your MAIA device.
#' @author Tjebo
#'
#' @param folder source folder which is searched for .tgz files. Default: workdirectory
#' @param incomplete set TRUE, if you want to see incomplete exams too
#' @param timeclass either 'date' (Date class, default) or 'datetime' (POSIXct), how to provide microperimetry test time
#' @return Data frame
#' @import dplyr
#' @import tidyr
#' @importFrom rlang .data
#' @examples
#' # read_maia_tgz(folder = file.path(getwd(), "data-raw"))
#' @export

## Function to extract test data from maia files (xml and json)
read_maia_tgz <- function(folder = getwd(),
                          incomplete = FALSE,
                          timeclass = "date") {
  # list of tgz files
  tgz_files <- get_tgz_files(folder = folder)
  # creates temporary path just for tgz files for unzipping
  tmpdir <- file.path(tempdir(), "tgzs")
  ## unzip all tgz files into temporary path
  lapply(tgz_files, unzip_tgz, tmpdir = tmpdir)
  ## list all folders in the temporary path
  tmpdir_list <- list.dirs(tmpdir, full.names = T)

  # XML data
  ## get all projection files from within all folders
  ls_xmls <- unlist(lapply(tmpdir_list,
    function(x) list.files(x, pattern = ".*projection.xml", full.names = TRUE)))
  ## makes vector of all the xml files with 'projection' but without diff in it
  ls_xml_nodiff <- ls_xmls[!grepl("diff", ls_xmls)]
  ## loop core xml extraction over each xml file
  ls_xml_dfs <- lapply(ls_xml_nodiff, function(x)
    make_df_each_xml(x, incomplete = incomplete))
  ## bind rows to one data frame with all xml data
  ## using bind_rows as dealing with possibly unequal number of colums
  ## for when some xml entries are missing
  df_xml <- bind_rows(ls_xml_dfs)

  # JSON data (PII)
  ## extract PII from json files and bind it to one data frame
  list_json <- unlist(lapply(tmpdir_list, function(x) list.files(x, pattern = ".json", full.names = TRUE)))
  list_pii <- lapply(list_json, extract_pii)
  df_pii <- do.call(rbind, list_pii)

  ## join test data and PII
  maiadata <- left_join(df_xml, df_pii, by = "patID")

  ## clean data frame and flip angles
  maiadata_clean <- clean_mp_data(maiadata, timeclass = timeclass)
  ## delete temporary directory
  unlink(tmpdir, recursive = TRUE)
  return(maiadata_clean)
}
## to be moved to testthat
# test <-
#   read_maia_tgz("/Users/tjebo/_temp_projects/microperimetr/data-raw")

#' clean_mp_data
#' @description cleaning up extracted data frame with mp test data and PII
#' @param x data frame
#' @param timeclass either 'date' (default) or 'datetime' (POSIXct)
#' @param incomplete set TRUE, if you want to see incomplete exams too
#' @return Data frame
#' @import xml2
#'
# lookup vector for test type
lu_test_type <- c(`0` = "mesopic", `1` = "mesopic", `6` = "cyan", `7` = "red")

clean_mp_data <- function(x, timeclass){
  clean_type <- x %>%
  dplyr::mutate(
    # replace integer test type with lookup
    test_type_new = lu_test_type[test_type],
    ## add column to identify exam as baseline or follow up
    ## could be better done by replacing "baselineID" == -1 with actual ID
    ## or maybe not, there might be incomplete BL exams that have complete FU exms??
    visit_type = ifelse(test_type == 2, "follow", "base"),
    ## see above
    baseID = ifelse(baseID == -1, testID, baseID),
    # if test is follow up, then replace baseline with lookup
    base_type_new = ifelse(test_type != 2, NA_character_, lu_test_type[base_type]),
    ## this can be safely coalesced, as the new columns contain matchin NA and non-NA values
    test_type = coalesce(.data$test_type_new, .data$base_type_new)) %>%
    # remove obsolete columns
    select(-test_type_new, -base_type_new, -base_type) %>%
    # rename id to stimID
    rename(stimID = id)

  ## clean date class
  if(!timeclass %in% c("date", "datetime"))
    stop("timeclass needs to be one of 'date' or 'datetime'")

  if (timeclass == "date") {
    clean_date <- clean_type %>%
      dplyr::mutate(testDate = lubridate::as_date(lubridate::ymd_hm(.data$testDate)))
  } else if (timeclass == "datetime") {
    clean_date <- clean_type %>%
      dplyr::mutate(testDate = lubridate::ymd_hm(.data$testDate))
  }
  ## Recode eye characters, flip angles of left eyes
  clean_angle <-
    clean_date %>%
    dplyr::mutate(
      eye = ifelse(eye == "Left", "l", "r"),
      angle = ifelse(.data$eye == "l", 180 - .data$angle, .data$angle),
      angle = ifelse(sign(.data$angle) == -1,
                 360 + .data$angle, .data$angle),
      angle = ifelse(.data$eccent == 0, 0, .data$angle)
      ) %>%
  ## arrange rows in meaningful way
    arrange(patID, testID, test_type, stimID) %>%
  ## arrange columns in meaningful way
  select("patID", "name", "first_name",
         "sex", "DOB", "age", "testID", "baseID", "test_type", "visit_type", "eye",
         "testDate", "avg_rctn", "stimID", "value", "eccent", "angle")
  return(clean_angle)
  }

#' @description core function to create data frame from xml attributes
#' @param x name of XML file
#' @param incomplete set TRUE, if you want to see incomplete exams too
#' @return Data frame
#' @import xml2
#' @import dplyr
#' @keywords internal
#'
make_df_each_xml <- function(x, incomplete) {
  xmlfile <- xml2::read_xml(x)
  xml_data <- list(xml2::as_list(xmlfile))[[1]][[1]]

  ## Stimuli extraction - define here what variables you want to pull out from the xml
  l_stimuli <- lapply(xml_data[[2]], function(x) {
    c(
      id = attributes(x)[["id"]],
      value = attributes(x)[["final_intensity"]],
      eccent = attributes(x)[["ray"]],
      angle = attributes(x)[["angle_deg"]]
    )
  })
  # make values positive, except the -1 (absolute scotomas)
  stimuli_df <- data.frame(t(sapply(l_stimuli, c)),
                           row.names = 1:length(l_stimuli),
                           stringsAsFactors = FALSE) %>%
    ## to be moved to separate data frame clean up function
    dplyr::mutate(id = as.integer(.data$id), value = as.integer(.data$value),
           eccent = round(as.numeric(.data$eccent),3),
           angle = round(as.numeric(.data$angle),3)) %>%
    dplyr::mutate(value = ifelse(.data$value == 1, -1, .data$value * (-1))
    )

  ## define here what else you want to pull out from the xml
  list1 <- list(
    patID = "PatientID",
    age = "Age",
    baseID = "ExamBaselineID",
    base_type = "ExamBaselineType",
    testID = "ExamID",
    Completed = "Completed",
    test_type = "ExamType",
    eye = "Eye",
    testDate = "DateTime",
    avg_rctn = "averageReactionTime_ms"
  )

  l_df <- nrow(stimuli_df)
  test_attributes <- lapply(list1, function(y) xml_data[[1]][[y]][[1]])
  ## removing NULL entries from list (where xml does not contain info)
  noNULL_attr <- Filter(function(x) !is.null(x), test_attributes)

  ### the following would probably be more interesting to keep as
  ## unique identifier separate from test ID values as a list
  ## should also be in a separate function
  attribute_df <- data.frame(t(sapply(noNULL_attr, c)), stringsAsFactors = FALSE)[rep(1, l_df), ]
  xml_df <- cbind(attribute_df, stimuli_df)
  row.names(xml_df) <- NULL
  # remove incomplete exams if argument is set to FALSE (default)
  if (incomplete == FALSE) {
    xml_df <- xml_df %>%
      filter(.data$Completed == 1) %>%
      dplyr::select(-'Completed')
  }
  return(xml_df)
}

## to be moved to testthat file when to publish in CRAN etc
# testing on specific XML.
# Found bug with xml files that might not contain specifit list entry, in particular baseline exam type to cyan and red
## (probably not required for non scotopic MAIA machines)
# test_xml <-
#   make_df_each_xml("/Users/tjebo/_temp_projects/microperimetr/data-raw/backup_maia-1054-patient180_20190401/180/835/projection.xml", incomplete = FALSE)
# str(test_xml)

#' @description ## List TGZ files
#' @param folder directory to look in
#' @keywords internal
#'
get_tgz_files <- function(folder) {
  tgz_files <- list.files(path = folder, pattern = ".tgz", full.names = TRUE)
  # check if any tgz file exists
  if (identical(tgz_files, character(0))) {
    stop("No tgz file in this directory")
  }
  tgz_files
}

#' @rdname read_maia_tgz
#' @description ## unzip tgz files
#' @param tgz_file file to extract
#' @param tmpdir temporary directory
#'
unzip_tgz <- function(tgz_file, tmpdir) { # tgz_file is element of tgz_files (each tgz file)
  ## the file name contains the patient ID (pulled here)
  patID <- stringr::str_extract(tgz_file, "(?i)(?<=patient)\\d+")
  ## the actual unpacking
  utils::untar(tgz_file, exdir = tmpdir)
  ## the tgz file contains an irrelevant sql file and a folder named by the patient ID
  tmpdir_patID <- paste0(tmpdir, "/", patID)
}

#' @rdname  read_maia_tgz
#' @description Extracts personally identifiable information from MAIA raw data.
#' @details
#' Internal function to extract name, dob and and sex from MAIA observers.
#' This data is stored in json files attached to the microperimetry files.
#' It is generally not recommended to use this function explicitely in order to keep personal identifiable data as safe as possible.
#' @param x jsonfile
#' @import dplyr
#' @importFrom jsonlite fromJSON
#' @author Tjebo
#' @return Data frame
#' @keywords internal
#'
extract_pii <- function(x) {
  ## makes list of json data, contains patient details
  JsonData <- jsonlite::fromJSON(x)
  ## list2: what you want to pull out of the .json file
  list2 <- list(patID = "id", name = "lastname", first_name = "firstname", sex = "gender", DOB = "birthdate")
  df_json <- data.frame(lapply(list2, function(z) JsonData[[1]][[z]]), stringsAsFactors = FALSE)
  ## Convert patID to char so that we can join it to XML data
  df_json$patID <- as.character(df_json$patID)
  return(df_json)
}


# to move into test_that etc
# ls_json <- read_maia_tgz2(folder = file.path(getwd(), "data-raw"))
#
# extract_pii(ls_json[[1]])


