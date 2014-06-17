library(data.table)
library(bit64)
library(lubridate)
library(stringr)
rm(list = ls())

system.time(source("Desktop/blabla.r"))

# read the data into memory
f <- fread("Downloads/problem_000000000000")
f
tables()

setkey(f, pl_user, pl_ts)
str(f)

# user dict
f[, user := as.numeric(factor(pl_user))]
user_dict <- data.table(f[, unique(pl_user)])
user_dict[, user := f[, unique(user)]]
setnames(user_dict, "V1", "pl_user")
user_dict
	# if there aren't a ton of unique users per file,
	# it may be fine to just keep writing to the same
	# dictionary (i.e. )
	
	
# old_user_dict <- fread("file", key = user)
# f <- old_user_dict[f]
# f[is.na(user), ]

# task type dict
f[, tt := as.numeric(factor(pl_task_type))]
tt_dict <- data.table(f[, unique(pl_task_type)])
tt_dict[, tt := f[, unique(tt)]]
setnames(tt_dict, "V1", "pl_task_type")
tt_dict

# exercise_dict
f[, exercise := as.numeric(factor(pl_exercise))]
exercise_dict <- data.table(f[, unique(pl_exercise)])
exercise_dict[, exercise := f[, unique(exercise)]]
setnames(exercise_dict, "V1", "pl_exercise")

f[, pl_hint_used := ifelse(pl_hint_used == "true", 1, 0)]
f[, pl_correct := ifelse(pl_correct == "true", 1, 0)]

fn <- f[, list(pl_correct, pl_ts, pl_time_taken, exercise,
               pl_hint_used, tt, user)]
fn[, pl_ts := str_sub(pl_ts, 1, -5)]
fn[, ts := as.numeric(fast_strptime(pl_ts, "%Y-%m-%d %H:%M:%OS"))]
fn[, pl_ts := NULL]
fn[, ts := ts - as.numeric(fast_strptime("2010-01-01", "%Y-%m-%d"))]
fn

f
f[, tt := NULL]
f[, exercise := NULL]
f[, user := NULL]

tables()

write.table(fn, "Downloads/problem_000000000000s5"))



## users
u <- fread("Downloads/user_000000000000")
setkey(u, user)
u[f]

sum(unique(f$pl_user) %in% u$user)
length(unique(f$pl_user))