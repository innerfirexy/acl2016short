# Merge the td, bf columns in entropy table back to the swbd.leader.ent.merged.rds
# Yang Xu
# 2/24/2016

library(RMySQL)
library(data.table)
library(dplyr)

df.old = readRDS('swbd.leader.ent.merged.rds')
dt.old = data.table(df.old)
setkey(dt.old, convID, globalID)

# read from db
# ssh yvx5085@brain.ist.psu.edu -i ~/.ssh/id_rsa -L 1234:localhost:3306
conn = dbConnect(MySQL(), host = '127.0.0.1', user = 'yang', port = 1234, password = "05012014", dbname = 'swbd')
sql = 'SELECT convID, globalID, td, bf FROM entropy'
df.db = dbGetQuery(conn, sql)

dt.db = data.table(df.db)
setkey(dt.db, convID, globalID)

# merge
dt.new = dt.old[dt.db, nomatch = 0]
# rename
dt.new = rename(dt.new, tdAdj = treeDepth, bfAdj = branchingFactor)
# save
saveRDS(dt.new, 'swbd.leader.new.rds')
