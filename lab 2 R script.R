install.packages("tidyverse")
install.packages("psych")
install.packages("readxl")
install.packages("gt")
install.packages("janitor")

library(tidyverse)
library(psych)
library(readxl)
library(gt)
library(janitor)

anxiety.df <- read_excel("./Attachment_Anxiety_Data.xlsx")

anxiety.clean.df <- anxiety.df |>
mutate(gender_char = case_when(Gender == 1 ~ "Male",
                               Gender == 2 ~ "Female"),
       ethnicity_char = case_when(Ethnicity == 1 ~ "white/Caucasian",
                                  Ethnicity == 2 ~ "Asian/Asian British",
                                  Ethnicity == 5 ~ "Other ethnic group",
                                  Ethnicity == 6 ~ "prefer not to say"),
       across(SA_1:SEst_10, as.integer))

SA_keys<- list (avg_SA=c("SA_1","SA_2","SA_3","SA_4","-SA_5","SA_6","SA_7","SA_8",
                       "-SA_9","SA_10","-SA_11","SA_12","SA_13","SA_14","SA_15","SA_16",
                       "SA_17","SA_18","SA_19","SA_20"))
AA_keys<- list (avg_AA=c("AA_1","AA_2","AA_3","AA_4","AA_5","AA_6","AA_7","AA_8",
                       "AA_9"))
SEst_keys<- list (avg_SEst=c("SEst_1","-SEst_2","SEst_3","SEst_4","-SEst_5","-SEst_6",
                       "SEst_7","-SEst_8","-SEst_9","SEst_10"))

SA_scores<-scoreItems(SA_keys, anxiety.clean.df, totals=F, min=0, max=4)
AA_scores<-scoreItems(AA_keys, anxiety.clean.df, totals=F, min=1, max=7)
SEst_scores<-scoreItems(SEst_keys, anxiety.clean.df, totals=F, min=1, max=4)

SA_scores.df   <- as.data.frame(SA_scores$scores)
AA_scores.df   <- as.data.frame(AA_scores$scores)
SEst_scores.df <- as.data.frame(SEst_scores$scores)

allscores.df <- cbind(SA_scores.df,AA_scores.df,SEst_scores.df)

anxiety.scored.df <- cbind(anxiety.clean.df, allscores.df)

head(anxiety.scored.df)

anxiety.final.df<- anxiety.scored.df|>
  select(URN, gender_char,ethnicity_char,avg_SA,avg_AA,avg_SEst)

demographic.table <- anxiety.final.df |>
  tabyl(gender_char, ethnicity_char)

demographic.gt <- demographic.table |>
  gt() |>
  tab_header(
    title = "Participant Demographics"
  )

####summary####
summary <- anxiety.final.df |>
  select(avg_AA, avg_SA, avg_SEst) |>
  describe()

summary$Scales <- c(
  "Attachment-Related Anxiety",
  "Social Interaction Anxiety",
  "Self-Esteem")

sample_size <- nrow(anxiety.final.df)
sample_size

alphas <- round(c(AA_scores$alpha, SA_scores$alpha,SEst_scores$alpha),2)

summary.table <- summary |>
  select(Scales, mean, median, sd, range)

summary.table <- cbind(summary.table, alpha = alphas)

score.table <- summary.table |>
  gt() |>
  tab_header(
    title = "Table 2 Summary Statistics for Attachment and Social Anxiety"
  ) |>
  cols_label(
    Scales = "Scale",
    mean = "Mean",
    median = "Median",
    sd = "SD",
    range = "Range",
    alpha = "Cronbach's Alpha"
  )

score.table

####


anxiety.female.df <- anxiety.scored.df |>
  filter(gender_char == "Female")

female_sample_size <- nrow(anxiety.female.df)
female_sample_size

female.summary <- anxiety.female.df |>
  select(avg_AA, avg_SA, avg_SEst) |>
  describe()

female.summary$Scales <- c(
  "Attachment-Related Anxiety",
  "Social Interaction Anxiety",
  "Self-Esteem")

female.summary.table <- female.summary |>
  select(Scales, mean, median, sd, range)

female.AA_scores <- scoreItems(AA_keys, anxiety.female.df,totals = F, min = 1, max = 7)
female.SA_scores <- scoreItems(SA_keys, anxiety.female.df,totals = F, min = 0, max = 4)
female.SEst_scores <- scoreItems(SEst_keys, anxiety.female.df,totals = F, min = 1, max = 4)

female.alphas <- round(c(
  female.AA_scores$alpha,
  female.SA_scores$alpha,
  female.SEst_scores$alpha), 2)

female.summary.table <- cbind(
  female.summary.table,
  alpha = female.alphas)

female.score.table <- female.summary.table |>
  gt() |>
  tab_header(
    title = "Table 3 Summary Statistics for Female Participants"
  ) |>
  cols_label(
    Scales = "Scale",
    mean = "Mean",
    median = "Median",
    sd = "SD",
    range = "Range",
    alpha = "Cronbach's Alpha"
  )

female.score.table
