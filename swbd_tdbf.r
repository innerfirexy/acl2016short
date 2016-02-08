# Analyze how tree depth and branching factor develops in Switchboard and plot
# Yang Xu
# 2/8/2016

library(lme4)
library(data.table)
library(dplyr)
library(ggplot2)

# read from RDS
df = readRDS('swbd.leader.ent.merged.rds')
df = rename(df, td = treeDepth, bf = branchingFactor)

df$role = 'initiator'
df[!df$byLeader,]$role = 'responder'

# td, bf vs. globalID
summary(lmer(td ~ globalID + (1|convID), df)) # t = 5.6
summary(lmer(bf ~ globalID + (1|convID), df)) # t = -6

# td, bf vs. inTopicID
summary(lmer(td ~ inTopicID + (1|convID) + (1|topicID), subset(df, role == 'initiator'))) # t = -0.4
summary(lmer(td ~ inTopicID + (1|convID) + (1|topicID), subset(df, role == 'responder'))) # t = 6.6

summary(lmer(bf ~ inTopicID + (1|convID) + (1|topicID), subset(df, role == 'initiator'))) # t = 2.7
summary(lmer(bf ~ inTopicID + (1|convID) + (1|topicID), subset(df, role == 'responder'))) # t = -11

# wordNum vs. inTopicID
summary(lmer(wordNum ~ inTopicID + (1|convID) + (1|topicID), subset(df, role == 'initiator'))) # t = -36.46
summary(lmer(wordNum ~ inTopicID + (1|convID) + (1|topicID), subset(df, role == 'responder'))) # t = 22.69


## examine the distr of td, bf among all speakers
dt = data.table(df)
setkey(dt, convID, speaker)

# add speakerID column
dt_spkrId = unique(dt[, .(convID, speaker)])
dt_spkrId$speakerID = 1:nrow(dt_spkrId)
dt = dt[dt_spkrId]

setkey(dt, speakerID)
dt_td = dt[, .(td = mean(td)), by = speakerID]
dt_bf = dt[, .(bf = mean(td)), by = speakerID]

distr_td = ggplot(dt_td, aes(td)) + geom_histogram()
plot(distr_td)

distr_bf = ggplot(dt_bf, aes(bf)) + geom_histogram()
plot(distr_bf)

# differentiate two speakers by whether td, bf are above or below mean values
dt_td$tdLvl = 'high'
dt_td[dt_td$td < mean(dt_td$td),]$tdLvl = 'low'
dt = dt[dt_td[,.(speakerID, tdLvl)]]

summary(lmer(td ~ globalID + (1|speakerID), subset(dt, tdLvl == 'high'))) # t = 2.4
summary(lmer(td ~ globalID + (1|speakerID), subset(dt, tdLvl == 'low'))) # t = 5.2
summary(lmer(td ~ inTopicID + (1|speakerID), subset(dt, tdLvl == 'high'))) # t = 3.8
summary(lmer(td ~ inTopicID + (1|speakerID), subset(dt, tdLvl == 'low'))) # t = 1.9

# p_td = ggplot(subset(dt, inTopicID <= 10),
#         aes(x = inTopicID, y = td, color = tdLvl, fill = tdLvl, lty = tdLvl)) +
#     stat_summary(fun.data = mean_cl_boot, geom = 'ribbon', alpha = .5) +
#     stat_summary(fun.y = mean, geom = 'line')
# plot(p_td)
