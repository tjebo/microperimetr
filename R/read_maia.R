#' read_maia
#' Extract MAIA raw data
#'
#' The function will extract the anonymised data from the raw .tgz output of your MAIA.
#'
#' @author Tjebo
#'
#' @param folder source folder which is searched for .tgz files. Default: workdirectory
#' @param incomplete set TRUE, if you want to see incomplete exams too
#' @param timeclass either 'date' (date class) or 'datetime' (POSIXct class), for date and time of test
#' @return Data frame
#'
#' @export

read_maia <- function(folder = getwd(), incomplete = FALSE, timeclass = 'datetime'){
  maiaR:::using('xml2', 'lubridate','utils', 'tidyverse')

  tgz_name <-  file.path(folder, list.files(folder)[grepl('.tgz',list.files(folder))]) #list of tgz files

  if(identical(tgz_name, character(0))) {
   stop('No tgz file in the specified directory')
 }
  pull_out <- function(tgz_element)  {
  #tgz_element is element of tgz_name (each tgz file), and pull_out will be used with lapply to perform the whole lot on every tgz file

  tmpdir <- tempdir()
  patID <- str_extract(tgz_element, "(?i)(?<=patient)\\d+")
  utils::untar(tgz_element, exdir = tmpdir)
  tmpdir_patID <- paste0(tmpdir,'/',patID)
  tmpdir_list <- list.files(paste0(tmpdir,'/',patID))
  ##dir of the files within the patID folder, makes object patID for the following code
  tmpdir_list <- tmpdir_list[!grepl('[[:alpha:]]', tmpdir_list)]
  ##makes vector of only the folders with exams in them

  loop_through_tmpdir <- function(x) {  ##the following function will create a list of list of dataframes for each exam in each subfolder
    list_dir <- paste0(tmpdir_patID,"/", x)
    list_dir_list<- list.files(list_dir)
    list_dir_xml<- list_dir_list[grepl('.*projection.xml', list_dir_list)]
    list_dir_xml<- list_dir_xml[!grepl('diff', list_dir_xml)]
    #makes vector of all the xml files with 'projection' but without diff in it

    make_df_each_xml <- function(x) {
      xmlfile <- xml2::read_xml(paste0(list_dir,'/', x))
      xml_data <- list(xml2::as_list(xmlfile))[[1]][[1]]

      ## define what you want to pull out for the stimuli of the xml

      l_stimuli <- lapply(xml_data[[2]], function(x) c(id = attributes(x)[['id']],
                                               fi = attributes(x)[['final_intensity']],
                                               ray = attributes(x)[['ray']],
                                               a_deg = attributes(x)[['angle_deg']]))


      stimuli_df <- data.frame(t(sapply(l_stimuli,c)), row.names = 1:length(l_stimuli), stringsAsFactors = FALSE) %>%
        mutate_all(as.integer) %>% mutate(fi = ifelse(fi == 1, -1, fi*(-1)))

      ## define what you want to pull out of the xml
      list1 <- list(patID = 'PatientID',
                    age = 'Age',
                    baseID = 'ExamBaselineID',
                    examID = 'ExamID',
                    Completed = 'Completed',
                    examType = 'ExamType',
                    eye = 'Eye',
                    examDate = 'DateTime',
                    avg_rctn = 'averageReactionTime_ms')

      l_df <- nrow(stimuli_df)
      exam_attributes <- lapply(list1, function(y) xml_data[[1]][[y]][[1]])
      attribute_df <- data.frame(t(sapply(exam_attributes,c)), stringsAsFactors = FALSE)[rep(1, l_df),]
      xml_df <- cbind(attribute_df, stimuli_df)
      row.names(xml_df) <- NULL

        if(incomplete == FALSE) {
        xml_df <- xml_df %>% filter(Completed == 1) %>% select(-Completed)
      }
    } ## end of function to loop through xml_projection_list
    lapply(list_dir_xml, make_df_each_xml)

  } ## end of function loop through tmp_dir

  lapply(tmpdir_list, loop_through_tmpdir)
} ##end of big pull_out() function to use for list of tgz files

list_all_exams <- lapply(tgz_name, pull_out) ## big end picture, result:
## list of list of lists of data frames for each xml for each exam folder for each patient folder,
## gives list of list of data frames

list_all_exams

data_all <- bind_rows(lapply(list_all_exams, function(x) bind_rows(lapply(x, bind_rows))))


#data_all

data_all <- data_all %>%
  rename(stimID = id) %>%
  arrange(patID, examID, stimID) %>%
  mutate(examType = case_when(examType == '0' ~ 'mesopic',
                              examType == '6' ~ 'cyan',
                              examType == '7' ~ 'red',
                              TRUE ~ examType))
data_all <- data_all %>%
  #filter(!examType %in% "2") %>%
  distinct(patID, examID, examType) %>%
  rename(type = examType) %>%
  left_join(data_all, ., by = c("patID", "baseID" = "examID")) %>%
  mutate(examType = coalesce(type, examType)) %>%
  select(-type)

if(timeclass == 'date'){
  data_all <- data_all %>% mutate(examDate = lubridate::as_date(lubridate::ymd_hm(examDate)))
} else if (timeclass == 'datetime'){
  data_all <- data_all %>% mutate(examDate = lubridate::ymd_hm(examDate))
}
 data_all

}
