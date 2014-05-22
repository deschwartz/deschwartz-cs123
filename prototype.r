### Prototyping Khan Academy Retention Analysis ###
setwd("~/Downloads/")
options(stringsAsFactors = F)
rm(list = ls())

install.packages("stringr")
library(stringr)
install.packages("ggplot2")
library(ggplot2)

# Read in the problem data
plog_raw <- str_c(readLines("problem_logs-000000000000", 
                            100001), 
                  " ")
plog_clean <- strsplit(plog_raw, 
                       ',(?=[^"]*(?:"[^"]*"[^"]*)*$)',
                       perl = T)
      # the right regex: only match commas that are followed
      #                  by an even # of quotation marks
      # finally!!!! :D
plog <- data.frame(matrix(unlist(plog_clean), 
                          nrow = 100001, 
                          byrow=T))
names(plog) <- plog[1,]
plog <- plog[-1,]
str(plog)

# tweak data types in problem data
plog$user <- as.numeric(plog$user)

plog$correct[plog$correct == "true"] <- 1
plog$correct[plog$correct == "false"] <- 0
plog$correct <- as.numeric(plog$correct)

plog$backup_timestamp <- as.numeric(plog$backup_timestamp)

plog$time_taken <- as.numeric(plog$time_taken)

plog$hint_used[plog$hint_used == "true"] <- 1
plog$hint_used[plog$hint_used == "false"] <- 0
plog$hint_used <- as.numeric(plog$hint_used)


# read in the video data
readLines("video_logs-000000000000", 5)
vlog <- read.csv("video_logs-000000000000", nrows = 100000)
str(vlog)
     # eliana still needs to give me the timestamp for these


# read in the user data (this is half of all users, EXCEPT those under age 13)
readLines("users000000000000", 5)
system.time(ulog <- read.csv("users000000000000"))
     # this is LONG: ~85 seconds
str(ulog)

# tweak data types for user data
ulog$coached[ulog$coached == "true"] <- 1
ulog$coached[ulog$coached != "1"] <- 0
ulog$coached <- as.numeric(ulog$coached)

ulog$is_teacher[ulog$is_teacher == "true"] <- 1
ulog$is_teacher[ulog$is_teacher != "1"] <- 0
ulog$is_teacher <- as.numeric(ulog$is_teacher)

ulog$coached_by_teacher[ulog$coached_by_teacher == "true"] <- 1
ulog$coached_by_teacher[ulog$coached_by_teacher != "1"] <- 0
ulog$coached_by_teacher <- as.numeric(ulog$coached_by_teacher)

ulog$coached_by_parent[ulog$coached_by_parent == "true"] <- 1
ulog$coached_by_parent[ulog$coached_by_parent != "1"] <- 0
ulog$coached_by_parent <- as.numeric(ulog$coached_by_parent)

ulog$is_parent[ulog$is_parent == "true"] <- 1
ulog$is_parent[ulog$is_parent != "1"] <- 0
ulog$is_parent <- as.numeric(ulog$is_parent)

ulog$index <- 1:nrow(ulog)
ulog$no_pretest <- ifelse(is.na(ulog$pretest_score), 1, 0)

str(ulog)

# explore!!
cor(ulog[,-8],use="everything")

summary(ulog)
     # of these users...
     #      ~13% are coached by someone
     #      ~10% are coached by a teacher in particular
     #      ~1.5% are coached by a parent in particular
     #      ~3% are themselves teachers **(should treat separately)**
     #      ~8% are themselves parents **(should treat separately)**
summary(ulog[ulog$is_teacher == 0 & ulog$is_parent == 0,])
     # of the non-parent, non-teacher users...
     #      ~14% are coached by someone (just a bit higher than before)
summary(ulog[ulog$coached == 0,])
length(which(ulog$coached_by_teacher == 1 & ulog$coached_by_parent == 1))
     # coached = coached_by_teacher + coached_by_parent - coached_by_teacher*coached_by_parent
     # there are 7,00 users coached by both teacher and parent.

numPT <- nrow(ulog)*c(0.03, 0.08)*2
nrow(ulog)*2 - sum(numPT)
     # number of teachers and parents (estimated) in full sample


system.time(naive <- lm(pretest_score ~ age_bucket, data = ulog))
     # pretty slow for 6.7 million observations... **will want to implement
     # ff and related models for full data (~ twice as big)**
     # .... took ~14 seconds (gasp!)
summary(naive)
     # reference category is empty age_bucket
     # the highest scoring group is actually 19-24 (probably b/c of college students)
tapply(ulog$pretest_score, ulog$age_bucket, mean, na.rm = T)

system.time(naive2 <- glm(coached ~ pretest_score, family = binomial, data = ulog))
     # glm took ~22 seconds, even longer as expected
summary(naive2)
1 / (1 + exp(-(-0.5 - 0.03*(mean(ulog$pretest_score, na.rm=T) + 5))))
     # someone with an average pretest_score has a ~17% chance of being coached.
     # 5 points higher and this model would estimate that your chance goes down to
     # 15%.

levels(as.factor(ulog$age_bucket))
summary(as.factor(ulog$coached))
summary(as.factor(ulog$is_parent))

dev.off()
ggplot(data = ulog, aes(x = pretest_score, fill = age_bucket)) +
  geom_density(alpha = 0.3)
     # also slow.
     # distribution-wise the different age groups are all pretty similar on pretest_score.
     # the main difference is that 13-15 has a narrower distribution.
     # also it's apparent that the pretest is not quite normal since it has right-skew...
     #     ... in other words, it's possible that the pretest could do a better job
     #         discriminating ability levels at the lower end of the spectrum
summary(as.factor(ulog$pretest_score), maxsum = 1000)

# Functions to get the time-summary data
      # for now just a template because the user ids for the problem logs don't
      # match the user ids for the user logs **(resolve eliana)**
plog_wrapper <- function(){
  for(user in ulog$user){
    plogs <- plog[plog$user == user,]
         # since I'm working over one user at a time, the map reduce code can be very similar
    
    join_date <- join_date(plogs)
    
    timeframes <- list(c(0,0), c(0,2), c(3,6), c(0,6), c(7,13), c(14, 20),
                       c(21, 27), c(0, 29), c(30, 59))
    num_problems_list <- list(d1_correct = c(), d1t3_correct = c(), d4t7_correct = c(),
                              w1_correct = c(), w2_correct = c(), w3_correct = c(),
                              w4_correct = c(), m1_correct = c(), m2_correct = c())
    for(i in 1:length(timeframes)) {
      num_problems_list[[i]] <- prop_correct(plogs, range = timeframes[[i]], join_date)
    }
         # repeat for other variables
  }
}