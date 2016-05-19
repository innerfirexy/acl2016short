# fit linear mixed models
# Yang Xu
# 2/27/2016

library(data.table)
library(lme4)
library(lmerTest)

df.swbd = readRDS('swbd.leader.new.rds')
df.bnc = readRDS('bnc.leader.tdbf.rds')

# change byLeader column to character
df.swbd$byLeader = as.character(df.swbd$byLeader)
df.swbd[df.swbd$byLeader == 'TRUE',]$byLeader = 'leader'
df.swbd[df.swbd$byLeader == 'FALSE',]$byLeader = 'follower'
setnames(df.swbd, 'byLeader', 'role')

df.bnc$byLeader = as.character(df.bnc$byLeader)
df.bnc[df.bnc$byLeader == 'TRUE',]$byLeader = 'leader'
df.bnc[df.bnc$byLeader == 'FALSE',]$byLeader = 'follower'
setnames(df.bnc, 'byLeader', 'role')


# swbd, td ~ inTopicID
m1 = lmer(td ~ inTopicID + (1|convID), df.swbd[role == 'leader', ])
summary(m1) # beta = -1.290e-01, t = -34.87, p = 0
coef(summary(m1))[,3]
m2 = lmer(td ~ inTopicID + (1|convID), df.swbd[role == 'follower', ])
summary(m2) # beta = 1.042e-01, t = 29.74, p = 0
summary(lmer(td ~ inTopicID + (1|convID), df.swbd)) # beta = -2.227e-02, t = -8.345, p < 0.001

# swbd, bf ~ inTopicID
m3 = lmer(bf ~ inTopicID + (1|convID), df.swbd[role == 'leader', ]) # beta = -1.820e-03, t = -27.2, p = 0
summary(m3)
m4 = lmer(bf ~ inTopicID + (1|convID), df.swbd[role == 'follower', ]) # beta = 2.141e-03, t = 30, p = 0
summary(m4)
summary(lmer(bf ~ inTopicID + (1|convID), df.swbd)) # beta = -2.092e-05, t = -0.414, p > 0.05

# swbd, wordNum ~ inTopicID
summary(lmer(wordNum ~ inTopicID + (1|convID), df.swbd[role == 'leader', ])) # beta = -3.626e-01, t = -36.7, p = 0
summary(lmer(wordNum ~ inTopicID + (1|convID), df.swbd[role == 'follower', ])) # beta = 1.883e-01, t = 22.45, p = 0
summary(lmer(wordNum ~ inTopicID + (1|convID), df.swbd)) # beta = -1.086e-01, t = -16.0, p <0.001

# swbd, tdAdj ~ inTopicID
summary(lmer(tdAdj ~ inTopicID + (1|convID), df.swbd[role == 'leader', ])) # beta = -2.242e-05, t = -0.1, p > 0.05
summary(lmer(tdAdj ~ inTopicID + (1|convID), df.swbd[role == 'follower', ])) # beta = 9.655e-04, t = 6.9, p < 0.001
summary(lmer(tdAdj ~ inTopicID + (1|convID), df.swbd)) # beta = 4.583e-04, t = 4.185, p < 0.001

# swbd, bfAdj ~ inTopicID
summary(lmer(bfAdj ~ inTopicID + (1|convID), df.swbd[role == 'leader', ])) # beta = 6.839e-05, t = 2, p < 0.05
summary(lmer(bfAdj ~ inTopicID + (1|convID), df.swbd[role == 'follower', ])) # beta = -2.879e-04, t = -11, p < 0.001
summary(lmer(bfAdj ~ inTopicID + (1|convID), df.swbd)) # beta = -9.077e-05, t = -4.612, p < 0.001


# bnc, td ~ inTopicID
summary(lmer(td ~ inTopicID + (1|convID), df.bnc[role == 'leader', ])) # beta = -2.988e-02, t = -13.15, p < 0.001
summary(lmer(td ~ inTopicID + (1|convID), df.bnc[role == 'follower', ])) # beta = 9.450e-03, t = 3.913, p < 0.001
summary(lmer(td ~ inTopicID + (1|convID), df.bnc)) # beta = -1.188e-02, t = -7.012***

# bnc, bf ~ inTopicID
summary(lmer(bf ~ inTopicID + (1|convID), df.bnc[role == 'leader', ])) # beta = -1.876e-03, t = -13.54, p < 0.001
summary(lmer(bf ~ inTopicID + (1|convID), df.bnc[role == 'follower', ])) # beta = 5.505e-04, t = 3.443, p < 0.001
summary(lmer(bf ~ inTopicID + (1|convID), df.bnc)) # beta = -7.818e-04, t = -7.301***

# bnc, wordNum ~ inTopicID
summary(lmer(wordNum ~ inTopicID + (1|convID), df.bnc[role == 'leader', ])) # beta = -1.658e-01, t = -12.69, p < 0.001
summary(lmer(wordNum ~ inTopicID + (1|convID), df.bnc[role == 'follower', ])) # beta = 1.198e-02, # t = 1.172, p > 0.05
summary(lmer(wordNum ~ inTopicID + (1|convID), df.bnc)) # beta = -8.610e-02, t = -9.773***

# bnc, tdAdj ~ inTopicID
summary(lmer(tdAdj ~ inTopicID + (1|convID), df.bnc[role == 'leader', ])) # beta =
summary(lmer(tdAdj ~ inTopicID + (1|convID), df.bnc[role == 'follower', ])) # beta =
summary(lmer(tdAdj ~ inTopicID + (1|convID), df.bnc)) # beta =

# bnc, bfAdj ~ inTopicID
summary(lmer(bfAdj ~ inTopicID + (1|convID), df.bnc[role == 'leader', ])) # beta =
summary(lmer(bfAdj ~ inTopicID + (1|convID), df.bnc[role == 'follower', ])) # beta =
summary(lmer(bfAdj ~ inTopicID + (1|convID), df.bnc)) # beta =




## test sentence complexity vs. globalID
summary(lmer(wordNum ~ globalID + (1|convID), df.swbd[role == 'leader',]))
summary(lmer(wordNum ~ globalID + (1|convID), df.swbd[role == 'follower',]))
summary(lmer(wordNum ~ globalID + (1|convID), df.swbd))

summary(lmer(td ~ globalID + (1|convID), df.swbd[role == 'leader',]))
summary(lmer(td ~ globalID + (1|convID), df.swbd[role == 'follower',]))
summary(lmer(td ~ globalID + (1|convID), df.swbd))


summary(lmer(wordNum ~ globalID + (1|convID), df.bnc[role == 'leader',]))
summary(lmer(wordNum ~ globalID + (1|convID), df.bnc[role == 'follower',]))
summary(lmer(wordNum ~ globalID + (1|convID), df.bnc))

summary(lmer(td ~ globalID + (1|convID), df.bnc[role == 'leader',]))
summary(lmer(td ~ globalID + (1|convID), df.bnc[role == 'follower',]))
summary(lmer(td ~ globalID + (1|convID), df.bnc))
