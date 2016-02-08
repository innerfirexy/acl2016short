# Analyze how tree depth and branching factor develops in Switchboard and plot
# Yang Xu
# 2/8/2016

library(RMySQL)
library(lme4)
library(data.table)
library(dplyr)

# ssh yvx5085@brain.ist.psu.edu -i ~/.ssh/id_rsa -L 1234:localhost:3306
conn = dbConnect(MySQL(), host = '127.0.0.1', user = 'yang', port = 1234, password = "05012014", dbname = 'swbd')
sql = 'SELECT convID, turnID, speaker, localID, globalID, wordNum, ent, treeDepth, branchingFactor,
    tileID, inTileID, tileStarter FROM entropy WHERE globalID <= 100'
df = dbGetQuery(conn, sql)

df = rename(df, td = treeDepth, bf = branchingFactor)
dt = data.table(df)

# add role column: 'initiator', 'responder'
dt$role = 'responder'
dt[dt$speaker == dt$tileStarter,]$role = 'initiator'
dt$role = as.factor(dt$role)

# td, bf vs. globalID
summary(lmer(td ~ globalID + (1|convID), dt)) # t = 6.4
summary(lmer(bf ~ globalID + (1|convID), dt)) # t = -7

# td, bf vs. inTileID
summary(lmer(td ~ inTileID + (1|convID) + (1|tileID), subset(dt, role == 'initiator')))
summary(lmer(td ~ inTileID + (1|convID) + (1|tileID), subset(dt, role == 'responder')))

summary(lmer(bf ~ inTileID + (1|convID) + (1|tileID), subset(dt, role == 'initiator')))
summary(lmer(bf ~ inTileID + (1|convID) + (1|tileID), subset(dt, role == 'responder')))

# wordNum vs. globalID
summary(lmer(wordNum ~ inTileID + (1|convID) + (1|tileID), subset(dt, role == 'initiator'))) # t = -9.63
summary(lmer(wordNum ~ inTileID + (1|convID) + (1|tileID), subset(dt, role == 'responder'))) # t = -16.54
