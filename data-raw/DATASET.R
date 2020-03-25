## norm_data_simone
norm_data_tzaridis <- read.csv(file = 'data-raw/norm_data.csv', header = TRUE, stringsAsFactors = FALSE)
usethis::use_data(norm_data_tzaridis)

# Norm
# read data and replace <0 with -1
data_wide <- read.csv("norm_val.csv", header=TRUE, sep=";", stringsAsFactors = FALSE) %>%
  mutate_at(.vars = vars(contains('X')), function(x) str_replace(x, '<0','-1'))
#data to long format
#splitting columns, removing "X" in the "position" variable
# reassigning data types
data_long <-
  data_wide %>%
  pivot_longer(names_to = 'position', values_to = 'value', X0_0:X10_180) %>%
  separate(position, sep = "_", into = c("eccent", "angle")) %>%
  separate(Type, sep =  "_", into  = c("testtype", "testnumber")) %>%
  mutate(eccent = as.integer(str_replace(eccent, "X", "")),
         angle = as.integer(angle),
         value = as.integer(value),
         testtype = as.factor(testtype),
         eye_id = paste(Patient.ID, Eye, sep="_")) %>%
  select(-Examination.ID, -Date.Time, -Exam.duration, -WrongPressureEvents, -Average.reaction.time, -Fixation.area.0.632, -Fixation.area.0.950, -Fixation.angle)

#calcualte difference of cyan and red exam
norm_data_pfau <- data_long  %>%
  pivot_wider(names_from = 'testtype', values_from = 'value') %>%
  mutate(CRDiff = Cyan - Red) %>%
  pivot_longer(names_to = 'testtype', values_to = 'value', cols = Mesopic:CRDiff)

usethis::use_data(norm_data_pfau)


