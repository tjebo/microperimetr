#' read_maia
#' Extract MAIA raw data
#' @name read_maia
#' @description
#' The function will extract the anonymised data from the raw .tgz patient backup files of your MAIA device.
#' @author Tjebo
#'
#' @param folder source folder which is searched for .tgz files. Default: workdirectory
#' @param incomplete set TRUE, if you want to see incomplete exams too
#' @param timeclass either 'date' (date class) or 'datetime' (POSIXct class), for date and time of test
#' @return Data frame
#' @import dplyr
#' @import tidyr
#' @importFrom rlang .data
#' @examples
#' # read_maia(folder = file.path(getwd(), "data-raw"))
#' @export

read_maia <- function(folder = getwd(), incomplete = FALSE, timeclass = "datetime") {
  # list of tgz files
  tgz_name <- file.path(folder, list.files(folder)[grepl(".tgz", list.files(folder))])
  # check if any tgz file exists
  if (identical(tgz_name, character(0))) {
    stop("No tgz file in the specified directory")
  }
  # pull_out will be lapplied on the list of tgz files
  pull_out <- function(tgz_element) { # tgz_element is element of tgz_name (each tgz file)
    tmpdir <- tempdir()
    patID <- stringr::str_extract(tgz_element, "(?i)(?<=patient)\\d+")
    utils::untar(tgz_element, exdir = tmpdir)
    tmpdir_patID <- paste0(tmpdir, "/", patID)
    ## dir of the files within the patID folder, make object patID for subenvironment
    tmpdir_list <- list.files(paste0(tmpdir, "/", patID))
    ## make vector of test folders
    tmpdir_list <- tmpdir_list[!grepl("[[:alpha:]]", tmpdir_list)]
    ## loop_through_tmpdir will create a list of lists of dataframes for each test in each subfolder
    loop_through_tmpdir <- function(x) {
      list_dir <- paste0(tmpdir_patID, "/", x)
      list_dir_list <- list.files(list_dir)
      list_dir_xml <- list_dir_list[grepl(".*projection.xml", list_dir_list)]
      # makes vector of all the xml files with 'projection' but without diff in it
      list_dir_xml <- list_dir_xml[!grepl("diff", list_dir_xml)]

      ## core function to create data frame from xml attributes
      make_df_each_xml <- function(x) {
        xmlfile <- xml2::read_xml(paste0(list_dir, "/", x))
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
          mutate(id = as.integer(.data$id), value = as.integer(.data$value),
                 eccent = round(as.numeric(.data$eccent),3),
                 angle = round(as.numeric(.data$angle),3)) %>%
          mutate(value = ifelse(.data$value == 1, -1, .data$value * (-1)),
                 angle = ifelse(sign(.data$angle) == -1,
                                360 + .data$angle, .data$angle)
                 )

        ## define here what else you want to pull out from the xml
        list1 <- list(
          patID = "PatientID",
          age = "Age",
          baseID = "ExamBaselineID",
          testID = "ExamID",
          Completed = "Completed",
          testtype = "ExamType",
          eye = "Eye",
          testDate = "DateTime",
          avg_rctn = "averageReactionTime_ms"
        )

        l_df <- nrow(stimuli_df)
        test_attributes <- lapply(list1, function(y) xml_data[[1]][[y]][[1]])
        attribute_df <- data.frame(t(sapply(test_attributes, c)), stringsAsFactors = FALSE)[rep(1, l_df), ]
        xml_df <- cbind(attribute_df, stimuli_df)
        row.names(xml_df) <- NULL
        # remove incomplete exams if argument is set to FALSE (default)
        if (incomplete == FALSE) {
          xml_df <- xml_df %>%
            filter(.data$Completed == 1) %>%
            select(-'Completed')
        }
      }
      ## end of function to make data frames looping through xml_projection_list
      ## happy loop
      lapply(list_dir_xml, make_df_each_xml)
    } ## end of function loop through tmp_dir

    lapply(tmpdir_list, loop_through_tmpdir)
  } ## end of big pull_out() function to use for list of tgz files

  ## big end picture, result:
  ## list of list of lists of data frames for each xml for each test folder for each patient folder,
  ## gives list of list of data frames
  list_all_exams <- lapply(tgz_name, pull_out)

  # data_all
  data_coll <-
    bind_rows(lapply(list_all_exams, function(x) bind_rows(lapply(x, bind_rows)))) %>%
    rename(stimID = id) %>%
    arrange(.data$patID, .data$testID, .data$stimID) %>%
    mutate(testtype = case_when(
      testtype == "0" ~ "mesopic",
      testtype == "6" ~ "cyan",
      testtype == "7" ~ "red",
      TRUE ~ .data$testtype
    ))
  data_test <- data_coll %>%
    # filter(!testtype %in% "2") %>%
    distinct(.data$patID, .data$testID, .data$testtype) %>%
    rename(type = .data$testtype)
  # return(data_test)
  # return(data_coll)
  data_all <- left_join(data_coll, data_test, by = c("patID", 'baseID' = "testID")) %>%
    mutate(testtype = coalesce(.data$type, .data$testtype),
           eye = if_else(.data$eye == 'Right', 'r', 'l')) %>%
    select(-"type")
  return(data_all)
  if (timeclass == "date") {
    data_all <- data_all %>%
      mutate(testDate = lubridate::as_date(lubridate::ymd_hm(.data$testDate)))
  } else if (timeclass == "datetime") {
    data_all <- data_all %>%
      mutate(testDate = lubridate::ymd_hm(.data$testDate))
  }
  # use get_names to retrieve sex details of patients. and join with main data frame
  pat_names <- get_names(folder = folder) %>%
    select('patID', 'sex') %>%
    mutate(patID = as.character(.data$patID))

  data_bind <-
    data_all %>%
    left_join(pat_names, by = 'patID') %>%
    select('patID', 'sex', everything())
  data_bind
}
