options(stringsAsFactors = F)
rm(list = ls())
# install and load important packages

install.packages(c("ggplot2", "data.table", "reshape2", "lme4", "bit64", "lubridate",
                   "stringr", "gridExtra"))
library(ggplot2)
library(data.table)
library(reshape2)
library(lme4)
library(bit64)
library(lubridate)
library(stringr)
library(gridExtra)

# would like to use
#      devtools::install_github("Kmisc", "kevinushey")
# for really fast data reshaping, 
# but devtools requires R >= 3.0.2 and the linux machines have 2.14.1 ... :(

# same problem for package: splitstackshape.
# in fact, R 2.14.1 doesn't even support data.table, 
# which is very important for this amount of data

# load in user background data; reformat types
setwd("~/Desktop/ka data/")
user0 <- fread("user_000000000000")
user1 <- fread("user_000000000001")
user2 <- fread("user_000000000002")

# change variable names
new_names <- c("id", "coached", "is_teacher", "coached_by_teacher", "is_parent", "coached_by_parent",
               "pretest_score", "age_bucket", "total_problem_seconds", "videos_completed")
setnames(user0, names(user0), new_names)
setnames(user1, names(user1), new_names)
setnames(user2, names(user2), new_names)

# set keys for fast searching (can perfrom binary search) and joining
setkey(user0, id)
setkey(user1, id)
setkey(user2, id)

# join the data
which(user0$id %in% user1$id)
which(user0$id %in% user2$id)
which(user1$id %in% user2$id)
     # there are about 450 users in all files... silly but ok

user <- unique(rbindlist(list(user0, user1, user2)))
user
str(user)
     # 13,866,185 unique users (age 13+)

tables()
rm(user0, user1, user2)

# convert to better data types
user[coached == "false", coached := "0"]
user[coached == "true", coached := "1"]
user[, coached := as.numeric(coached)]

user[is_teacher == "false", is_teacher := "0"]
user[is_teacher == "true", is_teacher := "1"]
user[, is_teacher := as.numeric(is_teacher)]

user[coached_by_teacher == "false", coached_by_teacher := "0"]
user[coached_by_teacher == "true", coached_by_teacher := "1"]
user[, coached_by_teacher := as.numeric(coached_by_teacher)]

user[is_parent == "false", is_parent := "0"]
user[is_parent == "true", is_parent := "1"]
user[, is_parent := as.numeric(is_parent)]

user[coached_by_parent == "false", coached_by_parent := "0"]
user[coached_by_parent == "true", coached_by_parent := "1"]
user[, coached_by_parent := as.numeric(coached_by_parent)]

user[pretest_score == "null", pretest_score := NA]
user[, pretest_score := as.numeric(pretest_score)]

user[, no_pretest := ifelse(is.na(pretest_score), 1, 0)]

summary(user)

# load in problem data; explore briefly
problem0 <- fread("problem_000000000000")
problem1 <- fread("problem_000000000001")
problem2 <- fread("problem_000000000002")

problem <- unique(rbindlist(list(problem0, problem1, problem2)))
sum(c(nrow(problem0), nrow(problem1), nrow(problem2))) - nrow(problem)
     # 62 repeated lines across all 3 files... not bad

rm(problem0, problem1, problem2)
tables()

summary(problem[, length(pl_problem_number), by = pl_user]$V1)
     # the median number of problems per uer is 48, mean is 220 (very skewed)
     # max is 23,770. decent sample
str(problem)

sum(problem$pl_user %in% user$id) - nrow(problem)
     # 0 people in the problem logs and not in the user logs. yay!

str(problem)
problem[, pl_correct := ifelse(pl_correct == "true", 1, 0)]
problem[, pl_hint_used := ifelse(pl_hint_used == "true", 1, 0)]

# drop variables I won't used (save memory)
problem[, pl_sha1 := NULL]
problem[, pl_problem_type := NULL]
problem[, pl_seed := NULL]
problem[, pl_problem_number := NULL]
tables()
     # saved a feww hundred mb


# join the user data to the problem logs
setkey(problem, pl_user)
setkey(user, id)

data <- user[problem]

rm(problem, user)
tables()

# get start date for each user
data[, pl_ts := str_sub(pl_ts, 1, -5)]
data[, pl_ts := fast_strptime(pl_ts, "%Y-%m-%d %H:%M:%OS")]
data[, start := min(pl_ts), by = id]

# classify problem logs into their intervals for each user
D1 = 1
D2T7 = 2
W2 = 3
W3T4 = 4
M2 = 5
M3 = 6
M4 = 7
M5 = 8
M6 = 9
M12 = 10
intervals <- c("d1", "d2t7", "w2", "w3t4", "m2", "m3", "m4", "m5", "m6")

data[, pl_tsn := as.numeric(pl_ts)]
data[, startn := as.numeric(start)]

summary((data$pl_tsn - data$startn)/(60*60*24))
qplot((data$pl_tsn - data$startn)/(60*60*24), geom = "histogram") +
  labs(title = "Days from each user's start date",
       x = "Days",
       y = "Number of problem logs")
     # the median number of days from start to pl_ts is 33, the mean is 65
     # the max is 311. (not bad -- though we won't be able to look at m12... just to m6 i think)
sum(data$pl_tsn - data$startn < 60*60*24*180)
     # this will exclude ~6 million problem logs, but that's ok (6 months is still interesting)

data[pl_tsn - startn < (60*60*24), interval := intervals[D1]]
data[pl_tsn - startn > (60*60*24), interval := intervals[D2T7]]
data[pl_tsn - startn > (60*60*24)*7, interval := intervals[W2]]
data[pl_tsn - startn > (60*60*24)*14, interval := intervals[W3T4]]
data[pl_tsn - startn > (60*60*24)*28, interval := intervals[M2]]
data[pl_tsn - startn > (60*60*24)*28*2, interval := intervals[M3]]
data[pl_tsn - startn > (60*60*24)*28*3, interval := intervals[M4]]
data[pl_tsn - startn > (60*60*24)*28*4, interval := intervals[M5]]
data[pl_tsn - startn > (60*60*24)*28*5, interval := intervals[M6]]
data <- data[pl_tsn - startn < (60*60*24)*28*6, ]
     # just keep problems up to 6 months from start date
data

data[, interval_num := match(interval, intervals)]

setkey(data, id, interval_num)

# make stacked data form
dt <- data.table(data[, rep(unique(id), each = 9)])
setnames(dt, "V1", "id")
dt[, interval := rep(intervals, times = length(unique(id)))]
dt[, interval_num := rep(1:9, times = length(unique(id)))]
setkey(dt, id, interval_num)

data_dict <- data[, 
                  list(interval[1], coached[1], is_teacher[1], coached_by_teacher[1], 
                       is_parent[1], coached_by_parent[1], pretest_score[1], 
                       age_bucket[1], total_problem_seconds[1], videos_completed[1], 
                       no_pretest[1], length(pl_correct), sum(pl_correct), 
                       sum(pl_time_taken), sum(pl_hint_used), start[1], {
                         rle <- data.frame(matrix(unlist(rle(pl_correct)), ncol = 2))
                         names(rle) <- c("run", "val")
                         ifelse(all(pl_correct == 1), 0, max(rle$run[rle$val == 0]))
                         }),
                  by = list(id, interval_num)]
     # I love data.table
setkey(data_dict, id, interval_num)
setnames(data_dict, names(data_dict), c("id", "interval_num", "interval", "coached", 
                                        "is_teacher", "coached_by_teacher", "is_parent", 
                                        "coached_by_parent", "pretest_score", "age_bucket", 
                                        "total_problem_seconds", "videos_completed",
                                        "no_pretest", "num_problems", "num_correct", 
                                        "total_time_taken", "num_hints", "start", "worst_streak"))
data_dict
     # 82,963 observations on 30,849 users
     # but there could be holes between time periods (implying 0s there)

summary(dt$pretest_score)
dt[is.na(pretest_score), pretest_score := 0]
dt <- data_dict[dt]
dt[, 
   attrit := ifelse(interval_num == sum(sapply(1:9, function(x) any(!is.na(num_problems[interval_num >= x])))),
                    1,
                    0),
   by = id]
dt[interval_num == 9, attrit := 0]
     # is this interval the last one that the user appears in?
     # (0 if the last interval is interval 9)
dt[, 
   keep := ifelse(interval_num > interval_num[attrit == 1], 
                  F, 
                  T),
   by = id]
dt <- dt[keep == T, ]
dt
     # 109,155 observations on 30,849 individuals

# replace NAs in time-invariant variables with correct value
dt[is.na(interval), 
   interval := intervals[interval_num]]
dt[, 
   coached := coached[1],
   by = id]
dt[, 
   is_teacher := is_teacher[1],
   by = id]
dt[, 
   coached_by_teacher := coached_by_teacher[1],
   by = id]
dt[, 
   is_parent := is_parent[1],
   by = id]
dt[, 
   coached_by_parent := coached_by_parent[1],
   by = id]
dt[, 
   age_bucket := age_bucket[1],
   by = id]
dt[, 
   total_problem_seconds := total_problem_seconds[1],
   by = id]
dt[, 
   videos_completed := videos_completed[1],
   by = id]
dt[, 
   no_pretest := no_pretest[1],
   by = id]
dt[, 
   start := start[1],
   by = id]

# replace NAs in time-variant variables with 0s
dt[is.na(num_problems),
   num_problems := 0]
dt[is.na(num_correct),
   num_correct := 0]
dt[is.na(total_time_taken),
   total_time_taken := 0]
dt[is.na(num_hints),
   num_hints := 0]
dt[is.na(worst_streak),
   worst_streak := 0]

# put NAs back in pretest_score
dt[pretest_score == 0, 
   pretest_score := NA]

# make variable for months since start of sample
dt[, 
   month := round(as.numeric(start - min(start))/(60*60*24*30))]

# make version of interval scaled to number of months
numeric_intervals <- c(1/30, mean(2:7)/30, mean(8:14)/30, mean(15:30)/30, 2, 3, 4, 5, 6)
dt[, 
   numeric_interval := numeric_intervals[interval_num]]
dt[, 
   numeric_interval_sq := numeric_interval^2]

dt[,
   has_pretest := 1 - no_pretest]

# make variables for early behavior
names(dt)
dt[,
   d1_num_problems := num_problems[interval == "d1"],
   by = id]
dt[,
   d1_num_problems_20p := as.numeric(num_problems[interval == "d1"] >= 20),
   by = id]
     # did you do more than 20 problems? (day 1)
dt[,
   d1_num_problems_20p_mc := d1_num_problems_20p - mean(d1_num_problems_20p)]
     # mean-centered version

dt[,
   d1_streak_6p := as.numeric(worst_streak[interval == "d1"] > 5),
   by = id]
     # did you get 6 or more wrong in a row? (day 1)
dt[,
   d1_streak_6p_mc := d1_streak_6p - mean(d1_streak_6p)]
     # mean-centered version

dt[,
   prop_correct := ifelse(num_problems == 0, 0, num_correct/num_problems)]
dt[,
   d1_half_or_more_wrong := as.numeric(prop_correct[interval == "d1"] < 0.5),
   by = id]
     # did you get more than half wrong? (day 1)
dt[,
   d1_half_or_more_wrong_mc := d1_half_or_more_wrong - mean(d1_half_or_more_wrong)]
     # mean-centered version

dt[, 
   d1_avg_time_per_problem := total_time_taken[interval == "d1"]/num_problems[interval == "d1"],
   by = id]
dt[,
   d1_too_fast := as.numeric(d1_avg_time_per_problem < 15)]
     # did you spend less than 15 seconds per question on average? (day 1)
dt[,
   d1_too_fast_mc := d1_too_fast - mean(d1_too_fast)]
     # mean-centered version

dt[,
   d1_used_hint := as.numeric(num_hints[interval == "d1"] > 0),
   by = id]
     # did you use a hint? (day 1)
dt[,
   d1_used_hint_mc := d1_used_hint - mean(d1_used_hint)]

dt[, 
   not_month1 := as.numeric(interval_num > 4)]

## finally ready for analysis

# main model
main_model <- glm(attrit ~ interval + d1_num_problems_20p_mc*interval + d1_half_or_more_wrong_mc + 
                    d1_streak_6p_mc + d1_too_fast_mc + d1_used_hint_mc,
                  data = dt,
                  family = binomial(link = cloglog))
summary(main_model)

# make table of hazard ratios (esp. for there's non-proportional hazards)
hazard_ratios <- data.frame(cbind(round(exp(-0.62296 + c(0, 0.93230, 0.76299, 0.75139, 0.73543, 
                                                         0.67135, 0.69802, 0.69593, 0.67173)), 
                                        2),
                                  round(exp(rep(0.19347, times = 9)), 2),
                                  round(exp(rep(-0.16955, times = 9)), 2),
                                  round(exp(rep(0.13550, times = 9)), 2),
                                  round(exp(rep(-0.04345, times = 9)), 2)))

pdf("hazard ratios.pdf")
grid.newpage()
hr_grob <- tableGrob(hazard_ratios[-1, ],
                     rows = c("Days 2-7", "Week 2", "Week 3-4", "Month 2", "Month 3",
                              "Month 4", "Month 5", "Month 6"),
                     cols = c("Did 20+\nproblems", "More than\nhalf wrong", "Streak of\n6+ wrong",
                              "Too fast", "Used a hint"))
title <- textGrob("Hazard Ratios for Dropping out of Khan Academy",
                  y = unit(0.5, "npc") + 0.5*grobHeight(hr_grob),
                  vjust = 0, gp = gpar(fontsize = 14))
grid.draw(gTree(children = gList(hr_grob, title)))
dev.off()

# plot hazard functions over time, grouped into facets for each categorical variable
mean_haz <- c(1 - exp(-exp(-0.620783 - c(0, 1.149855, 1.503631, 1.150014, 0.695536, 0.672811, 0.625965, 0.315425, 15.854332))))
num_prob_haz <- c(1 - exp(-exp(-0.620783 - c(0, 1.149855, 1.503631, 1.150014, 0.695536, 0.672811, 0.625965, 0.315425, 15.854332) +
                                 -0.622956 + c(0, 0.932298, 0.762986, 0.751388, 0.735425, 0.671354, 0.625965, 0.695927, 0.762986))))
half_wrong_haz <- c(1 - exp(-exp(-0.620783 - c(0, 1.149855, 1.503631, 1.150014, 0.695536, 0.672811, 0.625965, 0.315425, 15.854332) +
                                   0.19347)))
streak_haz <- c(1 - exp(-exp(-0.620783 - c(0, 1.149855, 1.503631, 1.150014, 0.695536, 0.672811, 0.625965, 0.315425, 15.854332) +
                               -0.169550)))
too_fast_haz <- c(1 - exp(-exp(-0.620783 - c(0, 1.149855, 1.503631, 1.150014, 0.695536, 0.672811, 0.625965, 0.315425, 15.854332) +
                                 0.135499)))
used_hint_haz <- c(1 - exp(-exp(-0.620783 - c(0, 1.149855, 1.503631, 1.150014, 0.695536, 0.672811, 0.625965, 0.315425, 15.854332) +
                                  -0.043452)))
all_haz <- c(mean_haz, num_prob_haz, half_wrong_haz, streak_haz, too_fast_haz, used_hint_haz)
haz_types <- rep(c("average", "did 20+ problems", "half wrong", "streak of 6+ wrong", "too fast", "used hint"), 
                 each = 9)
hz.to.gg <- data.frame(cbind(all_haz, haz_types, rep(1:9, times = 6)))
names(hz.to.gg) <- c("hazard", "user_type", "interval_num")
hz.to.gg$hazard <- as.numeric(hz.to.gg$hazard)
str(hz.to.gg)

ggplot(data = hz.to.gg, aes(x = interval_num, y = hazard,
                            group = user_type,
                            colour = user_type,
                            linetype = user_type)) +
  geom_line(lwd = 0.5) +
  scale_x_discrete(labels = intervals) +
  labs(title = "Estimated hazard functions for different types of users",
       x = "Time Interval",
       y = "Estimated probability of never returning to Khan Academy")


qplot(data[, as.numeric(max(pl_tsn) - min(pl_tsn))/(60*60*24), by = id]$V1) +
  labs(title = "Distribution of survival times in days",
       x = "Number of days",
       y = "Number of users")
