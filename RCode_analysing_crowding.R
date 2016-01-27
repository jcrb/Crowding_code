# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Author:   Andreas Halgreen Eiset
# Title:    A generic method for evaluating crowding in an emergency department
# Purpose:  R code for building the data sets and computing the analysis described in the article
# Licensed under GNU GENERAL PUBLIC LICENSE, Version 2, June 1991
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

# The following packages are needed for the provided code. If not already installed
# they should be so with the install.packages() command.

# library(dplyr)
# library(tidyr)
# library(lubridate)
# library(Hmisc)
# library(ggplot2)
# library(grid)
# library(gridExtra)

# Data should follow the specifications described in the supplied README and codebook files.

# Depending on how the data are stored it should be loaded as "df.pers" accordingly.
# Below is an example with data (crowding_data) stored as csv file.

df.pers <- read.csv(file = "./example_data.csv",
                      sep = ",",
                      stringsAsFactor = FALSE,
                      header = TRUE)

# Data frame with dates and time (dttm) intervals ----------------------------------------------------

# Every day in the study period must be divided into intervals of 30 minutes.
# Each patient included are followed until departure from the ED.
# Lines marked with "# NB!" are to be changed according to circumstances at the
# specific study site.

# Data frame with all relevant dates for all time intervals.
df.sum <- data.frame(
        dte = rep(
                seq(
                        from = as.Date(min(as.character(df.pers$arrival.dte))),
                        to = as.Date(max(as.character(df.pers$depart.dte))),
                        by = "day"
                ),
                each = 48)
)

# Data frame with factor containing all the time intervals. Note the use of 24-hour
# notation
time <- data.frame(tm.intrvl = rep(
        c("00:00-00:29", "00:30-00:59", "01:00-01:29", "01:30-01:59",
          "02:00-02:29", "02:30-02:59", "03:00-03:29", "03:30-03:59",
          "04:00-04:29", "04:30-04:59", "05:00-05:29", "05:30-05:59",
          "06:00-06:29", "06:30-06:59", "07:00-07:29", "07:30-07:59",
          "08:00-08:29", "08:30-08:59", "09:00-09:29", "09:30-09:59",
          "10:00-10:29", "10:30-10:59", "11:00-11:29", "11:30-11:59",
          "12:00-12:29", "12:30-12:59", "13:00-13:29", "13:30-13:59",
          "14:00-14:29", "14:30-14:59", "15:00-15:29", "15:30-15:59",
          "16:00-16:29", "16:30-16:59", "17:00-17:29", "17:30-17:59",
          "18:00-18:29", "18:30-18:59", "19:00-19:29", "19:30-19:59",
          "20:00-20:29", "20:30-20:59", "21:00-21:29", "21:30-21:59",
          "22:00-22:29", "22:30-22:59", "23:00-23:29", "23:30-23:59")))

# A unique id variable "period.id" is created.
df.sum <- cbind(df.sum, time) %>%
        unite(period.id, dte, tm.intrvl, sep = " - ", remove = FALSE)

rm(time)

# Data frame with all the relevant temporal subdivisions of the study period
dttm <- select(df.sum, period.id, dte, int = tm.intrvl) %>%
        separate(int, into = c("tmin1", "tmin2"), sep = "-") %>%
        mutate(dy = weekdays(as.Date(as.character(dte)))) %>%
        mutate(mnth = months(as.Date(as.character(dte)))) %>%
        mutate(dte = ymd(dte)) %>%
        arrange(period.id)

# Categorical variable with shifts. # NB! Time period and shift should be altered
# according to local circumstances. Below is code provided if a hospital works
# in two shift routines as well as code for four shift routine.

dttm$shift <- ifelse(dttm$tmin1 %in% c("07:00", "07:30", "08:00", "08:30",
                                       "09:00", "09:30","10:00", "10:30",
                                       "11:00", "11:30", "12:00", "12:30",
                                       "13:00", "13:30", "14:00", "14:30"),
                     "day", ifelse(dttm$tmin1 %in%
                                           c("15:00", "15:30", "16:00", "16:30",
                                             "17:00", "17:30","18:00", "18:30",
                                             "19:00", "19:30", "20:00", "20:30",
                                             "21:00", "21:30", "22:00", "22:30"),
                                   "evening", "night"))

# Example with only day and night shift. By removing the "#" this code can be
# used instead of the above provided.

#dttm$shift <- ifelse(dttm$tmin1 %in% c("07:00", "07:30", "08:00", "08:30",
 #                                      "09:00", "09:30","10:00", "10:30",
  #                                     "11:00", "11:30", "12:00", "12:30",
   #                                    "13:00", "13:30", "14:00", "14:30",
    #                                   "15:00", "15:30", "16:00", "16:30",
     #                                  "17:00", "17:30", "18:00", "18:30"),
      #               "day", "night")

# Example with four shifts. By removing the "#" this code can be
# used instead of the above provided.

#dttm$shift <- ifelse(dttm$tmin1 %in% c("07:00", "07:30", "08:00", "08:30",
 #                                      "09:00", "09:30", "10:00", "10:30",
  #                                     "11:00", "11:30", "12:00", "12:30"),
   #                  "day", ifelse(dttm$tmin1 %in%
    #                                       c("13:00", "13:30", "14:00", "14:30",
     #                                        "15:00", "15:30", "16:00", "16:30",
      #                                       "17:00", "17:30", "18:00", "18:30"),
       #                            "afternoon", ifelse(dttm$tmin1 %in% c("19:00", "19:30", "20:00", "20:30",
        #                                                                 "21:00", "21:30", "22:00", "22:30",
         #                                                                "23:00", "23:30", "00:00", "00:30"),
          #                                             "evening", "night")))

# Dichotomous variable dividing days into weekday/weekend
dttm$dicdy <- ifelse(dttm$dy %in% c("Saturday", "Sunday"),
                     "weekend", "weekday")

# Dichotomous variable dividing the year into summer/winter
dttm$season <- ifelse(as.character(dttm$mnth) %in%
                              c("January", "February", "March",
                                "October", "November", "December"),
                      "winter", "summer")

# In the above created data frame (dttm) the divisions are based on the time of
# the day. In a clinical setting it is often more relevant to divide the day
# according to shifts (sft). This means that "a new day" begins with day shift at 7 a.m.
# and ends with the night shift ending at 06.59 a.m.

# Thus, the first 14 observations of the obtained data (from 00:00 until
# 06:59) belongs to the previous day - that is outside the study period. These
# observations needs to be excluded. Each day has three eight hour shifts
# (i.e. 16 30-minutes), for the year of 2013 this amounts to 3*16*365 = 17520
# observations which is used in the example below.
# Lines marked with "# NB!" are to be changed according to study period.

dttmSft <- select(dttm, period.id, tmin1, tmin2, shift) %>%
        slice(1:c(3*16*365)) %>% # NB!
        cbind(dte.sft = as.factor(rep(
                seq(
                        from = as.Date("9999/01/01"), # NB!
                        to = as.Date("9999/12/31"), # NB!
                        by = "day"
                        ),
                each = 48))) %>%
        mutate(dte.sft = as.Date(dte.sft)) %>%
        mutate(dte.sft = Lag(dte.sft, shift = 14)) %>%
        slice(- c(1:14)) %>%
        mutate(dy.sft = weekdays(as.Date(as.character(dte.sft)))) %>%
        mutate(dicdy.sft = ifelse(dy.sft %in% c("Saturday", "Sunday"),
                                  "weekend", "weekday")) %>%
        unite(intrm, shift, dy.sft, sep = "_", remove = FALSE) %>%
        mutate(dicdy.sft = ifelse(intrm == "night_Friday", "weekend",
                                  ifelse(intrm == "night_Sunday", "weekday",
                                         ifelse(dicdy.sft == "weekday", "weekday",
                                                "weekend")))) %>%
        mutate(mnth.sft = months(as.Date(as.character(dte.sft)))) %>%
        mutate(season = ifelse(
                mnth.sft %in% c("April", "May", "June", "July", "August", "September"),
                "summer", "winter")
               ) %>%
        select(-intrm)

# Data set: The Blackbox model (arrival - departure)------------------------------

# Only patients in the relevant department ("6620378SKADE") in each 30-min. time interval
# are to be included. This should be modifyied to the department code of interest
pers <- df.pers[df.pers$first.location == "6620378SKADE" |
                            df.pers$last.location == "6620378SKADE", ] %>%
        unite(period.id, dte, tm.intrvl, sep = " - ", remove = FALSE)
row.names(pers) <- NULL

# Arrival of a patient is by definition the first interval the patient is
# registered in the ED. Likewise, departure is in the last registered interval.
# The code below creates a dummy variable each patient that denotes the interval
# of arrival and departure (= 1) for each patient. All intervals where that
# particular patient did not arrive (or depart) is marked with zero.

minrec <- group_by(pers, visit.id) %>%
        summarise(id = min(record.id))
pers <- mutate(pers, arrival = ifelse(record.id %in% minrec$id, 1, 0))

maxrec <- group_by(pers, visit.id) %>%
        summarise(id = max(record.id))
pers <- mutate(pers, departure = ifelse(record.id %in% maxrec$id, 1, 0))
rm(minrec, maxrec)

# Patients arriving before the dayshift of the first day of the study period
# must be excluded (see discussion for dttmSft above)

intrm1 <- filter(dttm, dte == min(dttm$dte) &
                          shift == "night" &
                          !tmin1 %in% c("23:00", "23:30")) %>%
        select(period.id)

intrm2 <- filter(pers, arrival == 1 & period.id %in% intrm1$period.id) %>%
        select(visit.id)

pers <- filter(pers, !visit.id %in% intrm2$visit.id)

rm(intrm1, intrm2)

# Data set with summed arrivals and departures (sumad) and computated
# queues ("qblack")

tm <- select(df.sum, period.id)

sumad <- group_by(pers, period.id) %>%
        summarise(arrivals = sum(arrival), departures = sum(departure)) %>%
        left_join(tm, ., by = "period.id") %>%
        arrange(period.id)
sumad[is.na(sumad)] <- 0

rm(tm)

intrm <- select(sumad, period.id, arrivals, departures) %>%
        cbind(data_frame(queue = cumsum(sumad$arrivals - sumad$departures))) %>%
        arrange(period.id) %>%
        select(queue) %>%
        mutate(queue = Lag(queue, shift = 1))
intrm[is.na(intrm)] <- 0

qblack <- select(sumad, period.id) %>%
        cbind(., intrm)

rm(intrm)

# Data set: Patients triaged 1/"red" (redpt) -------------------------------------

redpt.arr <- data.frame(redarr = tapply(pers[pers$triage.scr == "1", ]$arrival,
                              pers[pers$triage.scr == "1", ]$period.id,
                              sum))
# "NA" indicates there are no patients with triage = 1 in that time interval. "0" indicates
# that there are patients that were given triage score = 1, but that they did not arrived in this
# particular time interval.

redpt.arr <- data.frame(period.id = as.character(rownames(redpt.arr)), redpt.arr)

# Proportion of patients triaged 1/"red"
redpt.dep <- data.frame(reddep = tapply(pers[pers$triage.scr == "1", ]$departure,
                              pers[pers$triage.scr == "1", ]$period.id,
                              sum))
redpt.dep <- data.frame(period.id = as.character(rownames(redpt.dep)), redpt.dep)

red <- left_join(redpt.arr, redpt.dep, by = "period.id")
red[is.na(red)] <- 0

# Computation of queue length
intrm <- select(red, period.id, redarr, reddep) %>%
        cbind(data_frame(redq = cumsum(red$redarr - red$reddep))) %>%
        arrange(period.id) %>%
        select(redq) %>%
        mutate(redq = Lag(redq, shift = 1))
intrm[is.na(intrm)] <- 0

red <- select(red, period.id, redarr, reddep) %>%
        cbind(., intrm)

rm(intrm, redpt.arr, redpt.dep)

# Data set: Capasity (cpsty) -------------------------------------------------------
# -*- The following code will need some modifications according to the specific setting -*-

cpsty <- select(dttmSft, tmin1, shift, dicdy.sft, dy.sft, period.id)

# Bed capacity - in the case of the presented study 19 beds were disposable in
# the ED
cpsty$bed <- rep(19, length = nrow(cpsty))

# Number of nurses on weekday day shift until 12 o'clock
cpsty$nrs[cpsty$tmin1 %in% c("07:00", "07:30", "08:00", "08:30","09:00",
                             "09:30", "10:00", "10:30","11:00", "11:30") &
                  cpsty$dicdy.sft == "weekday"] <- 7
# ...weekday day shift after 12 o'clock (until 14.59 o'clock)
cpsty$nrs[cpsty$tmin1 %in% c("12:00", "12:30", "13:00",
                             "13:30", "14:00", "14:30") &
                  cpsty$dicdy.sft == "weekday"] <- 8
# ...weekday evening shift until 20 o'clock
cpsty$nrs[cpsty$tmin1 %in% c("15:00", "15:30", "16:00", "16:30", "17:00",
                             "17:30", "18:00", "18:30", "19:00", "19:30") &
                  cpsty$dicdy.sft == "weekday"] <- 8
# ...weekday evening shift after 20 o'clock (until 22.59 o'clock)
cpsty$nrs[cpsty$tmin1 %in% c("20:00", "20:30", "21:00",
                         "21:30", "22:00", "22:30") &
                  cpsty$dicdy.sft == "weekday"] <- 7
# ...weekday night shift (Sunday night are considered weekday)
cpsty$nrs[cpsty$shift == "night" & cpsty$dicdy.sft == "weekday"] <- 4

# Number of nurses on weekends day shift until 11 o'clock
cpsty$nrs[cpsty$tmin1 %in% c("07:00", "07:30", "08:00", "08:30",
                             "09:00", "09:30", "10:00", "10:30") &
                  cpsty$dicdy.sft == "weekend"] <- 7
# ...weekends day shift after 11 o'clock (until 14.59 o'clock)
cpsty$nrs[cpsty$tmin1 %in% c("11:00", "11:30", "12:00", "12:30",
                             "13:00", "13:30", "14:00", "14:30") &
                  cpsty$dicdy.sft == "weekend"] <- 8
# ...weekends evening shift until 20 o'clock
cpsty$nrs[cpsty$tmin1 %in% c("15:00", "15:30", "16:00", "16:30", "17:00",
                             "17:30", "18:00", "18:30", "19:00", "19:30") &
                  cpsty$dicdy.sft == "weekend"] <- 8
# ...weekends evening shift after 20 o'clock (until 22.59 o'clock)
cpsty$nrs[cpsty$tmin1 %in% c("20:00", "20:30", "21:00",
                             "21:30", "22:00", "22:30") &
                  cpsty$dicdy.sft == "weekend"] <- 7
# ...weekends night shift (Friday and Saturday night are considered part of the
# weekend)
cpsty$nrs[cpsty$shift == "night" & cpsty$dicdy.sft == "weekend"] <- 5

cpsty <- select(cpsty, -c(tmin1, dy.sft))

# Data set: Black box maxq  ---------------------------------------------------------
# -*- The following code will need minor modifications according to the specific setting -*-

# Intermediate data set: Max queue for every day and every shift
intrm1 <- select(dttmSft, period.id, dte.sft, shift) %>%
        left_join(., qblack, by = "period.id") %>%
        group_by(dte.sft, shift) %>%
        summarise(queue = max(queue))

# Intermediate data set: Max queue for patients triaged red for every day and every shift
intrm2 <- select(dttmSft, period.id, dte.sft, shift) %>%
        left_join(., red, by = "period.id") %>%
        group_by(dte.sft, shift) %>%
        summarise(maxredq = max(redq))

# Intermediate data set: Mean triage score of patients present in each time interval
intrm3 <- distinct(pers, visit.id) %>%
        group_by(period.id) %>%
        summarise(tri.scr = mean(triage.scr, na.rm = TRUE)) %>%
        left_join(select(dttmSft, dte.sft, shift, period.id), .,
                  by = "period.id") %>%
        group_by(dte.sft, shift) %>%
        summarise(mntriscr = max(tri.scr, na.rm = TRUE))

# The final data set
maxq <- left_join(intrm1, intrm2, by = c("dte.sft", "shift")) %>%
        left_join(., intrm3, by = c("dte.sft", "shift")) %>%
        mutate(dy.sft = weekdays(dte.sft)) %>%
        unite(intrm, shift, dy.sft, sep = "_", remove = FALSE) %>%
        mutate(dicdy.sft = ifelse(dy.sft %in% c("Saturday", "Sunday"),
                                  "weekend", "weekday")) %>%
        mutate(dicdy.sft = ifelse(intrm == "night_Friday", "weekend",
                                  ifelse(intrm == "night_Sunday", "weekday",
                                         ifelse(dicdy.sft == "weekday", "weekday",
                                                "weekend")))) %>%
        mutate(mnth.sft = months(as.Date(as.character(dte.sft)))) %>%
        mutate(season = ifelse(
                mnth.sft %in% c("April", "May", "June", "July", "August", "September"),
                "summer",
                "winter")) %>%
        select(-intrm) %>%
        ungroup() %>%
        filter(!dte.sft == "9999-01-01") # to allow for steady state the first 24-hours are ignored


rm(intrm1, intrm2, intrm3)

# Table 2: Characteristics of the emergency department and the patients -----------------------------------------------------
# -*- The following code will need minor modifications according to the specific setting -*-

dta <- left_join(pers, dttmSft, by = "period.id") %>%
        filter(! is.na(dte.sft))
# Likly to produce a warning message

# Arrivals
sum(dta$arrival)
summarise(group_by(dta, dicdy.sft), sum(arrival))

# Proportion of arrivals registered as trauma patients (local code "DT068")
summarise(filter(dta, diagnosis == "DT068"),
          sum(arrival))
summarise(filter(group_by(dta, dicdy.sft), diagnosis == "DT068"),
          sum(arrival))

# Proportion of arrivals triaged "red"/1
summarise(filter(dta, triage.scr == 1),
          sum(arrival))
summarise(filter(group_by(dta, dicdy.sft), triage.scr == 1),
          sum(arrival))

# Proportion of arrivals that were female
summarise(group_by(dta, sex), sum(arrival))
summarise(group_by(dta, dicdy.sft, sex), sum(arrival))

# Proportion of arrivals younger than 18 years old
summarise(filter(dta, age < 18),
          sum(arrival))
summarise(filter(group_by(dta, dicdy.sft), age < 18),
          sum(arrival))

# Proportion of arrivals older than 65 years old
summarise(filter(dta, age > 65),
          sum(arrival))
summarise(filter(group_by(dta, dicdy.sft), age > 65),
          sum(arrival))

rm(dta)

# Figure 3: Arrivals, departures and the resulting queues  ---------------------------------------------------------
# -*- The following code will need some modifications according to the specific setting -*-

# Find the time with the maximum queue length

qblack[which.max(qblack$queue), ]
# Below is worked through with the example_data.csv provided
#       period.id                       queue
#13224  9999-10-03 - 11:30-11:59        21
# The maximum queue length during the study period were 21 patients.
# It occured March 10th in the time period 11:30 - 11:59.

# Data frames including preceeding and following days
# ...queue data
dtaq <-  inner_join(qblack,
                    select(dttmSft, dy.sft, dte.sft, tmin1, tmin2, period.id),
                    by = "period.id") %>%
        filter(dte.sft >= "9999-10-01", dte.sft <= "9999-10-05")

# ...arrival data
dtaa <-  select(sumad, arrivals, period.id) %>%
        inner_join(select(dttmSft, dy.sft, dte.sft, tmin1, tmin2, period.id),
                   by = "period.id") %>%
        filter(dte.sft >= "9999-10-01", dte.sft <= "9999-10-05")

# ... departure data
dtad <-  select(sumad, departures, period.id) %>%
        inner_join(select(dttmSft, dy.sft, dte.sft, tmin1, tmin2, period.id),
                   by = "period.id") %>%
        filter(dte.sft >= "9999-10-01", dte.sft <= "9999-10-05")

# Set up coordinate system with box indicating crowding (makes no sense with
# the made up example_data.csv)
rect1 <- data.frame(xmin = -Inf, xmax = Inf, ymin = 19, ymax = Inf)
gq <- ggplot(dtaq, aes(period.id, queue, group = 1)) +
        geom_vline(xintercept = seq(from = 1,
                                    to = length(dtaq$period.id),
                                    by = 2), colour = "white") +
        geom_rect(data = rect1, aes(xmin = xmin, xmax = xmax,
                                   ymin = ymin, ymax = ymax),
              fill = "red", alpha = 0.6, inherit.aes = FALSE)
rm(rect1)

# Plot arrivals, departures and resulting queue "on top" of the red box
gq + geom_point(aes(colour = "Queue Length")) +
        geom_line(aes(colour = "Queue Length")) +
        geom_line(data = dtaa,
                  aes(period.id, arrivals, group = 1, colour = "Arrivals"),
                  ) +
        geom_point(data = dtaa,
                   aes(period.id, arrivals, colour = "Arrivals"),
                   ) +
        geom_line(data = dtad,
                  aes(period.id, departures, group = 1, colour = "Departures"),
                  alpha = 0.6) +
        geom_point(data = dtad,
                   aes(period.id, departures, colour = "Departures"),
                   alpha = 0.6) +
        scale_colour_manual(values = c("Queue Length" = "black",
                                       "Arrivals" = "darkgreen",
                                       "Departures" = "blue"),
                            breaks = c("Arrivals", "Queue Length", "Departures")) +
        scale_x_discrete(breaks = c("9999-10-01 - kl. 00:00-00:29",
                                    "9999-10-02 - kl. 00:00-00:29",
                                    "9999-10-03 - kl. 00:00-00:29",
                                    "9999-10-04 - kl. 00:00-00:29",
                                    "9999-10-05 - kl. 00:00-00:29")) +
        scale_y_continuous(breaks = c(0, 5, 10, 15, 19, 20, 25, 30, 35, 40)) +
        labs(y = "Number of patients",
             x = "Date & time") +
        theme(plot.title = element_text(size = 20),
              legend.title = element_blank(),
              legend.position = "bottom")

rm(dtaq, dtaa, dtad, gq)

# Figure 4: Boxplot of arrivals and queue length --------------------------

g <- ggplot(maxq, aes(shift, queue)) +
        geom_boxplot(aes(fill = dy.sft)) +
        labs(title = "MAXIMUM QUEUES", x = "Shift", y = "Queue Lenght") +
        scale_x_discrete(labels = c("Day", "Evening", "Night")) +
        theme(legend.title = element_blank(),
              legend.text = element_text(size = 20),
              axis.text.x = element_text(size = 15),
              axis.title.x = element_text(size = 20, vjust = -0.8),
              axis.text.y = element_text(size = 15),
              axis.title.y = element_text(size = 20, vjust = 1.5),
              plot.title = element_text(size = 20)) +
        guides(fill = guide_legend(keywidth = 2, keyheight = 1.5))

dta1 <- select(dttmSft, dte.sft, dy.sft, shift, period.id) %>%
        left_join(., sumad, by = "period.id")

dta1$dy.sft <- factor(dta1$dy.sft, levels = c("Monday", "Tuesday", "Wednesday",
                                              "Thursday", "Friday", "Saturday",
                                              "Sunday"))

g1  <- ggplot(dta1, aes(shift, arrivals)) +
        geom_boxplot(aes(fill = dy.sft)) +
        scale_y_continuous(breaks = c(0, 5, 10)) +
        labs(title = "ARRIVALS", y = " Number of Arrivals") +
        theme(legend.title = element_blank(),
              legend.text = element_text(size = 20),
              axis.text.x = element_blank(),
              axis.title.x = element_blank(),
              axis.text.y = element_text(size = 15),
              axis.title.y = element_text(size = 20, vjust = 1.5),
              plot.title = element_text(size = 20)) +
        guides(fill = guide_legend(keywidth = 2, keyheight = 1.5))


dta2 <- select(dttmSft, dte.sft, dy.sft, shift, period.id) %>%
        left_join(., qblack, by = "period.id")

dta2$dy.sft <- factor(dta2$dy.sft, levels = c("Monday", "Tuesday", "Wednesday",
                                              "Thursday", "Friday", "Saturday",
                                              "Sunday"))
g2 <- ggplot(dta2, aes(shift, queue)) +
        geom_boxplot(aes(fill = dy.sft)) +
        labs(title = "ALL QUEUES", y = "Queue Length") +
        theme(legend.title = element_blank(),
              legend.text = element_text(size = 20),
              axis.text.x = element_blank(),
              axis.title.x = element_blank(),
              axis.text.y = element_text(size = 15),
              axis.title.y = element_text(size = 20, vjust = 1.5),
              plot.title = element_text(size = 20)) +
        guides(fill = guide_legend(keywidth = 2, keyheight = 1.5))

library(gridExtra)
grid.arrange(g1, g2, g)

rm(g, g2, g1, dta1, dta2)

# Figure 5: Empirical cumulative distribution function of maximum  --------

g <- ggplot(maxq, aes(queue))
ecd <- g + aes(colour = shift) +
        facet_grid(dicdy.sft ~ .) +
        stat_ecdf() +
        labs(x = "Maximum Queue Length in the ED pr. day") +
        labs(y = "Cumulative Probability") +
        #ggtitle("EMPIRICAL CUMULATIVE DISTRIBUTION FUNCTION, MAXIMUM QUEUE SIZE") +
        #theme(plot.title = element_text(lineheight = .8, face = "bold")) +
        geom_vline(xintercept = 19, linetype = 3, size = 0.8, colour = "red") +
        scale_colour_discrete(guide = guide_legend(title = "Shift")) +
        theme(legend.text = element_text(size = 10),
              legend.title = element_text(size = 10),
              legend.position = "bottom",
              #    axis.text.x = element_text(size = 15),
              axis.title.x = element_text(size = 10, vjust = -0.3),
              #  axis.text.y = element_text(size = 15),
              axis.title.y = element_text(size = 10, vjust = 1.5),
              #plot.title = element_text(size = 20),
              strip.text.y = element_text(size = 10, face = "bold"))
#strip.text.x = element_text(size = 10, face = "bold")) #+
#guides(fill = guide_legend(keywidth = 2, keyheight = 2))
ecd

# To get the specific y-coordinates (% risk) at a specified threshold (here 19)
filter(ggplot_build(ecd)$data[[1]], x == 19)
# Likly to produce a warning message

rm(g, ecd)

# Table 3: Characteristics of maximum queue lengths per day ---------------

summarise(group_by(maxq, shift, dicdy.sft),
          min(queue),
          max(queue),
          median(queue),
          IQR(queue))

# Likly to produce a warning message

# Figure 6: Correlation of max queue length between shifts and days -------
# -*- The following code will need some modifications according to the specific setting -*-

dta <- select(maxq, -c(4:7, 9)) %>%
        distinct(dte.sft, shift) %>%
        spread(shift, queue) %>%
        arrange(dte.sft) %>%
        mutate(lag.night = Lag(night, shift = 1)) %>%
        mutate(lag.evening = Lag(evening, shift = 1)) %>%
        mutate(lag.evening2 = Lag(evening, shift = 2)) %>%
        mutate(lag.day = Lag(day, shift = 1)) %>%
        mutate(lag.day3 = Lag(day, shift = 3)) %>%
        mutate(lag.day4 = Lag(day, shift = 4))


# - * - # day shift vs evening shift # - * - #

g1 <- dta %>% group_by(dte.sft) %>%
        ggplot(aes(day, evening)) +
        geom_jitter() +
        labs(x = "Maximum Queue Length, Day Shift") +
        labs(y = "Maximum Queue Length, Evening Shift") +
        labs(title = "Shifts \n Day & Following Evening Shift \n Spearman's rank correlation rho = X, p-value < X") +
        theme(plot.title = element_text(face = "bold"))

cor.test(dta$day, dta$evening, method = "spearman", alternative = "two.sided")
# Produces a warning message if ties exists


# - * - # evening shift vs night shift # - * - #

g2 <- dta %>% group_by(dte.sft) %>%
        ggplot(aes(evening, night)) +
        geom_jitter() +
        labs(x = "Maximum Queue Length, Evening Shift") +
        labs(y = "Maximum Queue Length, Night Shift") +
        labs(title = "Shifts \n Evening & Following Night Shift \n Spearman's rank correlation rho = X, p-value = X") +
        theme(plot.title = element_text(face = "bold"))

cor.test(dta$evening, dta$night, method = "spearman", alternative = "two.sided")
# Produces a warning message if ties exists

# - * - # night shift vs day shift of next day # - * - #

g3 <- dta %>% group_by(dte.sft) %>%
        ggplot(aes(lag.night, day)) +
        geom_jitter() +
        labs(x = "Maximum Queue Length, Night Shift") +
        labs(y = "Maximum Queue Length, Day Shift") +
        labs(title = "Shifts \n Night & Following Day Shift \n Spearman's rank correlation rho = X, p-value = X") +
        theme(plot.title = element_text(face = "bold"))

cor.test(dta$lag.night, dta$day, method = "spearman", alternative = "two.sided")
# Produces a warning message if ties exists

# - * - # Arrivals; days following each other # - * - #

dta1 <- left_join(select(dttmSft, dte.sft, shift, period.id), sumad,
                  by = "period.id") %>%
        filter(! dte.sft == "9999-01-01") %>%
        group_by(dte.sft) %>%
        summarise(arrivals = sum(arrivals)) %>%
        mutate(lag.day = Lag(arrivals, shift = 1))

library(ggplot2)

g <- dta1 %>% ggplot(aes(arrivals, lag.day)) +
        geom_jitter() +
        labs(x = "Arrivals One Day") +
        labs(y = "Arrivals The Following Day") +
        labs(title = "Arrivals \n Days Following Each Other \n Spearman's rank correlation rho = X, p-value = X") +
        theme(plot.title = element_text(face = "bold"))

cor.test(dta1$arrivals, dta1$lag.day, method = "spearman", alternative = "two.sided")
# Produces a warning message if ties exists

grid.arrange(g1, g2, g3, g, ncol = 2)

rm(dta, dta1, g, g1, g2, g3)
