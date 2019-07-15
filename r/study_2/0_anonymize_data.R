# script to anonymize the participants data set (cannot be rerun as we do not
# provide the unanonymized data)
library(tidyverse)

# Read data
participants <- read_csv("data/study_2/participants.csv", na = "NULL")

# exclude all identifying information 
participants <- participants %>%
  select(partid, cond_order, cond_soepframe, age, sex, yearsofedu, job, income,
         imc1, imc2, qual_focused, qual_effort, status, device, remembered,
         similarity, duration)

# write data
write_csv(participants, "data/study_2/participants_anonymous.csv")
