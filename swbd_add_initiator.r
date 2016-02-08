# Add topicRole column to entropy table
# which indicates the speaker is the 'initiator' or 'responder' of the current
# topic episode
# Yang Xu
# 2/8/2016

library(RMySQL)
library(data.table)
library(dplyr)

# ssh yvx5085@brain.ist.psu.edu -i ~/.ssh/id_rsa -L 1234:localhost:3306
conn = dbConnect(MySQL(), host = '127.0.0.1', user = 'yang', port = 1234, password = "05012014", dbname = 'swbd')
sql = 'SELECT convID, turnID, speaker, localID, globalID, wordNum, ent, treeDepth, branchingFactor,
    tileID, inTileID, tileStarter FROM entropy'
df = dbGetQuery(conn, sql)

dt = data.table(df)
setkey(dt, convID, tileID)

# For each topic episode, i.e., unique (convID, tileID) combination,
# find its initiator, and the other speaker is naturally the responder
# Notes:
#   the first episode is neglected
#   for within-turn topic boundary, initiator is the first speaker of the topic
#   for between-turn topic boundary, initiator is the first speaker whose utterance consists of
#       more than 5 words
