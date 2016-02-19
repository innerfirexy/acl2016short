# Add the td bf columns in entropy_DEM100 of bnc db to bnc.leader.rds
# Yang Xu
# 2/19/2016

library(RMySQL)
library(data.table)

# ssh yvx5085@brain.ist.psu.edu -i ~/.ssh/id_rsa -L 1234:localhost:3306
conn = dbConnect(MySQL(), host = '127.0.0.1', user = 'yang', port = 1234, password = "05012014", dbname = 'bnc')
sql = 'SELECT convID, globalID, td, bf FROM entropy_DEM100'
df.db = dbGetQuery(conn, sql)

dt.db = data.table(df.db)
setkey(dt.db, convID, globalID)

# read original df
df = readRDS('bnc.leader.rds')
dt = data.table(df)
setkey(dt, convID, globalID)

# merge
dt.new = dt[dt.db, nomatch = 0]

# compute the tdAdj and bfAdj
setkey(dt.new, wordNum)
dt.new[, tdAdj := td / mean(td), by = wordNum]
dt.new[, bfAdj := bf / mean(bf), by = wordNum]

# save
saveRDS(dt.new, 'bnc.leader.tdbf.rds')
