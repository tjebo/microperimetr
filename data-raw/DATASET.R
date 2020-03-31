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
         eye = if_else(Eye == 'OD','r','l')) %>%
  select(patID = Patient.ID, eye, age = Age, sex, lens, testID = Examination.ID, contains('Fixation'),
         wrong = WrongPressureEvents, testtype, testnumber, avg_rctn = Average.reaction.time,
         eccent, angle, value) %>%
#calculate difference of cyan and red exam
  select(-contains('Fixation'), -avg_rctn,-testID,-wrong) %>%
  pivot_wider(names_from = 'testtype', values_from = 'value') %>%
  mutate(cr_diff = cyan - red) %>%
  pivot_longer(names_to = 'testtype', values_to = 'value', cols = mesopic:cr_diff) %>%
  mutate(testtype = factor(testtype, levels = c("mesopic", "cyan", "red", "cr_diff")),
         testID = paste(patID, eye, testtype, testnumber, sep = '_')) %>%
  arrange(testID, eccent, angle) %>%
  group_by(testID) %>%
  mutate(stimID = seq_along(testID)) %>%
  ungroup()


usethis::use_data(norm_data, overwrite = TRUE)

data_model <-
  norm_data %>%
  select(-testID, stimID)  %>%
  mutate(testnumber = paste0("E", testnumber)) %>%
  pivot_wider(names_from = 'testnumber', values_from = 'value') %>%
  group_by(testtype) %>%
  mutate(MeanSens = (E1+E2)/2, position = as.factor(paste(eccent, angle, sep = '_'))) %>%
  ungroup() %>%
  select(patID, eye, sex, age, lens, testtype, position, MeanSens)

# adding the output of linear model to the norm data
pred_norm <- compare_norm(norm_data) # that takes a long time!
pred_shape <- pred_norm %>%
  mutate(testnumber = str_sub(testID, -1, -1)) %>%
  select(patID, eye, testtype, testnumber, eccent, angle, stimID, value, fit, lwr, upr) %>%
  mutate(testtype = factor(testtype, levels = c("mesopic", "cyan", "red", "cr_diff")),
         testID = paste(patID, eye, testtype, testnumber, sep = '_'))

MD_PSD_norm <- MD_PSD(pred_shape)

summary_MDPSD <- microperimetR:::MD_PSD_norm %>% mutate(testID = str_replace(testID, 'cr_diff','crdiff')) %>%
  separate(testID, c('patID', 'eye', 'testtype', 'testnumber')) %>%
  mutate(testtype = str_replace(testtype, 'crdiff','cr_diff')) %>%
  group_by(patID, testtype) %>%
  summarise_at(.vars = vars(MeanDev, PSD), .funs = mean) %>%
  ungroup %>%
  split(.$testtype)
# data_model <- microperimetR:::data_model
# pred_shape <- microperimetR:::pred_shape
# MD_PSD_norm <- microperimetR:::MD_PSD_norm
usethis::use_data(data_model, pred_shape, MD_PSD_norm, summary_MDPSD, internal = TRUE, overwrite = TRUE)


