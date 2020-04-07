# read pfau data and replace <0 with -1
data_wide <- read.csv("data-raw/norm_pfau.csv", header=TRUE, sep=";", stringsAsFactors = FALSE) %>%
  mutate_at(.vars = vars(contains('X')), function(x) str_replace(x, '<0','-1'))
#data to long format
#splitting columns, removing "X" in the "position" variable
# reassigning data types
norm_data <-
  data_wide %>%
  pivot_longer(names_to = 'position', values_to = 'value', X0_0:X10_180) %>%
  separate(position, sep = "_", into = c("eccent", "angle")) %>%
  separate(Type, sep =  "_", into  = c("testtype", "testnumber")) %>%
  mutate(eccent = as.integer(str_replace(eccent, "X", "")),
         angle = as.integer(angle),
         value = as.integer(value),
         testtype = tolower(testtype),
         lens = replace_na(Lens, 'natural'),
         lens = if_else(lens != "pp", "natural", "pseudo"),
         sex = if_else(Sex == 'Male', 'm','f'),
         eye = if_else(Eye == 'OD','r','l'),
         angle = ifelse(.data$eye == 'l', 180- .data$angle, .data$angle),
         angle = ifelse(sign(.data$angle) == -1,
                        360 + .data$angle, .data$angle),
         angle = ifelse(.data$eccent == 0, 0, .data$angle)
         ) %>%
  select(patID = Patient.ID, eye, age = Age, sex, lens, testID = Examination.ID, contains('Fixation'),
         wrong = WrongPressureEvents, testtype, testnumber, avg_rctn = Average.reaction.time,
         eccent, angle, value) %>%
#calculate difference of cyan and red exam
  select(-contains('Fixation'), -avg_rctn,-testID,-wrong) %>%
  pivot_wider(names_from = 'testtype', values_from = 'value') %>%
  mutate(cr_diff = cyan - red) %>%
  pivot_longer(names_to = 'testtype', values_to = 'value', cols = mesopic:cr_diff) %>%
  mutate(testID = paste(patID, eye, testtype, testnumber, sep = '_')) %>%
  arrange(testID, eccent, angle) %>%
  group_by(testID) %>%
  mutate(stimID = seq_along(testID)) %>%
  ungroup()

usethis::use_data(norm_data, overwrite = TRUE)

data_model <- norm_data %>%
  select(-'testID') %>%
  mutate(testnumber = paste0("E", .data$testnumber)) %>%
  pivot_wider(names_from = "testnumber", values_from = "value") %>%
  mutate(
    MeanSens = (.data$E1 + .data$E2) / 2,
    position = paste(.data$eccent, .data$angle, sep = "_"),
    position_fac = factor(position, levels = vec_normpositions)
  ) %>%
  select(-"E1", -"E2")

usethis::use_data(data_model, overwrite = TRUE)

cor_maia <- norm_data %>%
  mutate(testnumber = paste0("E", .data$testnumber)) %>%
  select(-'testID') %>%
  pivot_wider(names_from = 'testnumber', values_from = 'value') %>%
  mutate(diff_val = .data$E1-.data$E2, avg = mean(.data$E1+.data$E2)) %>%
  group_by(.data$testtype) %>%
  summarise(subj_sd = stats::sd(.data$diff_val, na.rm = TRUE), CoR = 1.96*sqrt(2)*.data$subj_sd)

usethis::use_data(cor_maia, overwrite = TRUE)

norm_compared <- compare(norm_data) # that takes a while!

norm_vs_fit <- norm_compared %>%
  bind_rows() %>%
  select(-'test_unq')

norm_mpstats_raw <- mpstats(norm_compared)

norm_mpstats <- norm_mpstats_raw %>%
  as.data.frame %>%
  tibble::rownames_to_column('rowID') %>%
  mutate(rowID = gsub('cr_diff', 'crdiff', rowID),
         rowID = gsub("_[^_]*$","", rowID)) %>%
  separate(rowID, c('patID', 'eye', 'testtype', 'testnumber'))

usethis::use_data(norm_mpstats, norm_vs_fit, internal = TRUE, overwrite = TRUE)



