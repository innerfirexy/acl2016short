# plot swbd.leader.ent.merged.rds and bnc.leader.rds
# Yang Xu
# 2/15/2016

library(ggplot2)
library(dplyr)

df.swbd = readRDS('swbd.leader.ent.merged.rds')
df.bnc = readRDS('bnc.leader.tdbf.rds')

# change byLeader column to character
df.swbd$byLeader = as.character(df.swbd$byLeader)
df.swbd[df.swbd$byLeader == 'TRUE',]$byLeader = 'initiator'
df.swbd[df.swbd$byLeader == 'FALSE',]$byLeader = 'responder'
df.swbd = rename(df.swbd, role = byLeader)

df.bnc$byLeader = as.character(df.bnc$byLeader)
df.bnc[df.bnc$byLeader == 'TRUE',]$byLeader = 'initiator'
df.bnc[df.bnc$byLeader == 'FALSE',]$byLeader = 'responder'
df.bnc = rename(df.bnc, role = byLeader)



# plot swbd
# entropy vs inTopicID
p.swbd1 = ggplot(subset(df.swbd, inTopicID <= 10), aes(x = inTopicID, y = ent)) +
    stat_summary(fun.data = mean_cl_boot, geom = 'ribbon', alpha = .5, aes(fill = role)) +
    stat_summary(fun.y = mean, geom = 'line', aes(lty = role)) +
    scale_x_continuous(breaks = 1:10) +
    theme(legend.position = c(.8, .1)) +
    xlab('within-topic position of sentence') + ylab('per-word entropy') +
    guides(fill = guide_legend(title = 'speaker role'), lty = guide_legend(title = 'speaker role'))
pdf('swbd-ent_vs_inTopicID_roles.pdf', 5, 5)
plot(p.swbd1)
dev.off()

# adjusted entropy vs inTopicID
p.swbd2 = ggplot(subset(df.swbd, inTopicID <= 10), aes(x = inTopicID, y = entc, group = role)) +
    stat_summary(fun.data = mean_cl_boot, geom = 'ribbon', alpha = .5, aes(fill = role)) +
    stat_summary(fun.y = mean, geom = 'line', aes(lty = role)) +
    scale_x_continuous(breaks = 1:10) +
    theme(legend.position = c(.8, .1)) +
    xlab('within-topic position of sentence') + ylab('adjusted entropy') +
    guides(fill = guide_legend(title = 'speaker role'), lty = guide_legend(title = 'speaker role'))
pdf('swbd-entAdj_vs_inTopicID_roles.pdf', 5, 5)
plot(p.swbd2)
dev.off()

# wordNum vs inTopicID
p.swbd3 = ggplot(subset(df.swbd, inTopicID <= 10), aes(x = inTopicID, y = wordNum)) +
    stat_summary(fun.data = mean_cl_boot, geom = 'ribbon', alpha = .5, aes(fill = role)) +
    stat_summary(fun.y = mean, geom = 'line', aes(lty = role)) +
    scale_x_continuous(breaks = 1:10) +
    theme(legend.position = c(.8, .1)) +
    xlab('within-topic position of sentence') + ylab('sentence length (number of words)') +
    guides(fill = guide_legend(title = 'speaker role'), lty = guide_legend(title = 'speaker role'))
pdf('swbd-wordNum_vs_inTopicID_roles.pdf', 5, 5)
plot(p.swbd3)
dev.off()

# treeDepth vs inTopicID
p.swbd4 = ggplot(subset(df.swbd, inTopicID <= 10), aes(x = inTopicID, y = treeDepth)) +
    stat_summary(fun.data = mean_cl_boot, geom = 'ribbon', alpha = .5, aes(fill = role)) +
    stat_summary(fun.y = mean, geom = 'line', aes(lty = role)) +
    scale_x_continuous(breaks = 1:10) +
    theme(legend.position = c(.8, .1)) +
    xlab('within-topic position of sentence') + ylab('adjusted tree depth') +
    guides(fill = guide_legend(title = 'speaker role'), lty = guide_legend(title = 'speaker role'))
pdf('swbd-treeDepth_vs_inTopicID_roles.pdf')
plot(p.swbd4)
dev.off()

# branchingFactor vs inTopicID
p.swbd5 = ggplot(subset(df.swbd, inTopicID <= 10), aes(x = inTopicID, y = branchingFactor)) +
    stat_summary(fun.data = mean_cl_boot, geom = 'ribbon', alpha = .5, aes(fill = role)) +
    stat_summary(fun.y = mean, geom = 'line', aes(lty = role)) +
    scale_x_continuous(breaks = 1:10) +
    theme(legend.position = c(.8, .1)) +
    xlab('within-topic position of sentence') + ylab('adjusted tree depth') +
    guides(fill = guide_legend(title = 'speaker role'), lty = guide_legend(title = 'speaker role'))
pdf('swbd-branchingFactor_vs_inTopicID_roles.pdf')
plot(p.swbd5)
dev.off()



# plot bnc
# entropy vs inTopicID
p.bnc1 = ggplot(subset(df.bnc, inTopicID <= 10), aes(x = inTopicID, y = ent)) +
    stat_summary(fun.data = mean_cl_boot, geom = 'ribbon', alpha = .5, aes(fill = role)) +
    stat_summary(fun.y = mean, geom = 'line', aes(lty = role)) +
    scale_x_continuous(breaks = 1:10) +
    theme(legend.position = c(.8, .1)) +
    xlab('within-topic position of sentence') + ylab('per-word entropy') +
    guides(fill = guide_legend(title = 'speaker role'), lty = guide_legend(title = 'speaker role'))
pdf('bnc-ent_vs_inTopicID_roles.pdf', 5, 5)
plot(p.bnc1)
dev.off()

# adjusted entropy vs inTopicID
p.bnc2 = ggplot(subset(df.bnc, inTopicID <= 10), aes(x = inTopicID, y = entc)) +
    stat_summary(fun.data = mean_cl_boot, geom = 'ribbon', alpha = .5, aes(fill = role)) +
    stat_summary(fun.y = mean, geom = 'line', aes(lty = role)) +
    scale_x_continuous(breaks = 1:10) +
    theme(legend.position = c(.8, .2)) +
    xlab('within-topic position of sentence') + ylab('adjusted entropy')
plot(p.bnc2)

# wordNum vs inTopicID
p.bnc3 = ggplot(subset(df.bnc, inTopicID <= 10), aes(x = inTopicID, y = wordNum)) +
    stat_summary(fun.data = mean_cl_boot, geom = 'ribbon', alpha = .5, aes(fill = role)) +
    stat_summary(fun.y = mean, geom = 'line', aes(lty = role)) +
    scale_x_continuous(breaks = 1:10) +
    theme(legend.position = c(.8, .1)) +
    xlab('within-topic position of sentence') + ylab('sentence length (number of words)')
plot(p.bnc3)

# td vs inTopicID
p.bnc4 = ggplot(subset(df.bnc, inTopicID <= 10), aes(x = inTopicID, y = td)) +
    stat_summary(fun.data = mean_cl_boot, geom = 'ribbon', alpha = .5, aes(fill = role)) +
    stat_summary(fun.y = mean, geom = 'line', aes(lty = role)) +
    scale_x_continuous(breaks = 1:10) +
    theme(legend.position = c(.8, .1)) +
    xlab('within-topic position of sentence') + ylab('tree depth')
plot(p.bnc4)

# bf vs inTopicID
p.bnc5 = ggplot(subset(df.bnc, inTopicID <= 10), aes(x = inTopicID, y = bf)) +
    stat_summary(fun.data = mean_cl_boot, geom = 'ribbon', alpha = .5, aes(fill = role)) +
    stat_summary(fun.y = mean, geom = 'line', aes(lty = role)) +
    scale_x_continuous(breaks = 1:10) +
    theme(legend.position = c(.8, .1)) +
    xlab('within-topic position of sentence') + ylab('branching factor')
plot(p.bnc5)

# tdAdj vs inTopicID
p.bnc6 = ggplot(subset(df.bnc, inTopicID <= 10), aes(x = inTopicID, y = tdAdj)) +
    stat_summary(fun.data = mean_cl_boot, geom = 'ribbon', alpha = .5, aes(fill = role)) +
    stat_summary(fun.y = mean, geom = 'line', aes(lty = role)) +
    scale_x_continuous(breaks = 1:10) +
    theme(legend.position = c(.8, .1)) +
    xlab('within-topic position of sentence') + ylab('adjusted tree depth')
plot(p.bnc6)

# bfAdj vs inTopicID
p.bnc7 = ggplot(subset(df.bnc, inTopicID <= 10), aes(x = inTopicID, y = bfAdj)) +
    stat_summary(fun.data = mean_cl_boot, geom = 'ribbon', alpha = .5, aes(fill = role)) +
    stat_summary(fun.y = mean, geom = 'line', aes(lty = role)) +
    scale_x_continuous(breaks = 1:10) +
    theme(legend.position = c(.8, .1)) +
    xlab('within-topic position of sentence') + ylab('adjusted branching factor')
plot(p.bnc7)





## try plotting swbd and bnc together in one figure
df.swbd.tmp = select(df.swbd, ent, entc, wordNum, inTopicID, role)
df.swbd.tmp$corpus = 'Switchboard'
df.swbd.tmp$group = 'Switchboard: initiator'
df.swbd.tmp[df.swbd.tmp$role == 'responder',]$group = 'Switchboard: responder'

df.bnc.tmp = select(df.bnc, ent, entc, wordNum, inTopicID, role)
df.bnc.tmp$corpus = 'BNC'
df.bnc.tmp$group = 'BNC: initiator'
df.bnc.tmp[df.bnc.tmp$role == 'responder',]$group = 'BNC: responder'

df.all = rbind(df.swbd.tmp, df.bnc.tmp)

# get ggplot default colors
gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}
my_colors = rep(rev(gg_color_hue(2)), 2)

# entropy vs inTopicID
p.together1 = ggplot(subset(df.all, inTopicID <= 10), aes(x = inTopicID, y = ent)) +
    stat_summary(fun.data = mean_cl_boot, geom = 'ribbon', alpha = .5, aes(fill = group)) +
    stat_summary(fun.y = mean, geom = 'line', aes(lty = group)) +
    stat_summary(fun.y = mean, geom = 'point', aes(shape = group)) +
    scale_x_continuous(breaks = 1:10) +
    theme(legend.position = c(.8, .15)) +
    xlab('within-topic position of sentence') + ylab('per-word entropy') +
    guides(fill = guide_legend(title = 'group'),
        lty = guide_legend(title = 'group'),
        shape = guide_legend(title = 'group')) +
    scale_fill_manual(values = my_colors) +
    scale_linetype_manual(values = c(1,3,1,3)) +
    scale_shape_manual(values = c(1,1,4,4))
pdf('all-ent_vs_inTopicID_roles.pdf', 5, 5)
plot(p.together1)
dev.off()

# tree depth vs inTopicID
p.together2 = ggplot(subset(df.all, inTopicID <= 10), aes(x = inTopicID, y = ent)) +
