# plot swbd.leader.ent.merged.rds and bnc.leader.rds
# Yang Xu
# 2/15/2016

library(ggplot2)
library(data.table)

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

# td vs inTopicID
p.swbd4 = ggplot(subset(df.swbd, inTopicID <= 10), aes(x = inTopicID, y = td)) +
    stat_summary(fun.data = mean_cl_boot, geom = 'ribbon', alpha = .5, aes(fill = role)) +
    stat_summary(fun.y = mean, geom = 'line', aes(lty = role)) +
    scale_x_continuous(breaks = 1:10) +
    theme(legend.position = c(.8, .1)) +
    xlab('within-topic position of sentence') + ylab('tree depth') +
    guides(fill = guide_legend(title = 'speaker role'), lty = guide_legend(title = 'speaker role'))
pdf('swbd-td_vs_inTopicID_roles.pdf')
plot(p.swbd4)
dev.off()

# bf vs inTopicID
p.swbd5 = ggplot(subset(df.swbd, inTopicID <= 10), aes(x = inTopicID, y = bf)) +
    stat_summary(fun.data = mean_cl_boot, geom = 'ribbon', alpha = .5, aes(fill = role)) +
    stat_summary(fun.y = mean, geom = 'line', aes(lty = role)) +
    scale_x_continuous(breaks = 1:10) +
    theme(legend.position = c(.8, .1)) +
    xlab('within-topic position of sentence') + ylab('branching factor') +
    guides(fill = guide_legend(title = 'speaker role'), lty = guide_legend(title = 'speaker role'))
pdf('swbd-bf_vs_inTopicID_roles.pdf')
plot(p.swbd5)
dev.off()

# tdAdj vs inTopicID, NTD
p.swbd6 = ggplot(subset(df.swbd, inTopicID <= 10), aes(x = inTopicID, y = tdAdj)) +
    stat_summary(fun.data = mean_cl_boot, geom = 'ribbon', alpha = .5, aes(fill = role)) +
    stat_summary(fun.y = mean, geom = 'line', aes(lty = role)) +
    scale_x_continuous(breaks = 1:10) +
    scale_linetype_manual(values = c('leader' = 1, 'follower' = 3)) +
    theme(legend.position = c(.8, .15)) +
    xlab('Within-topic position of sentence') + ylab('Normalized tree depth') +
    guides(fill = guide_legend(title = 'speaker role'), lty = guide_legend(title = 'speaker role'))
pdf('swbd-tdAdj_vs_inTopicID_roles.pdf', 5, 5)
plot(p.swbd6)
dev.off()

# bfAdj vs inTopicID, NBF
p.swbd7 = ggplot(subset(df.swbd, inTopicID <= 10), aes(x = inTopicID, y = bfAdj)) +
    stat_summary(fun.data = mean_cl_boot, geom = 'ribbon', alpha = .5, aes(fill = role)) +
    stat_summary(fun.y = mean, geom = 'line', aes(lty = role)) +
    scale_x_continuous(breaks = 1:10) +
    scale_linetype_manual(values = c('leader' = 1, 'follower' = 3)) +
    theme(legend.position = c(.8, .8)) +
    xlab('Within-topic position of sentence') + ylab('Normalized branching factor') +
    guides(fill = guide_legend(title = 'speaker role'), lty = guide_legend(title = 'speaker role'))
pdf('swbd-bfAdj_vs_inTopicID_roles.pdf', 5, 5)
plot(p.swbd7)
dev.off()




## plot tdAdj and bfAdj together in one graph
library(gridExtra)

pdf('swbd_tdAdj_bfAdj.pdf', 4.5, 9)
grid.arrange(p.swbd6, p.swbd7, nrow = 2, heights = c(1, 1))
dev.off()

pdf('swbd_tdAdj_bfAdj_horizontal.pdf', 9, 4.5)
grid.arrange(p.swbd6 + ggtitle('NTD'), p.swbd7 + ggtitle('NBF'), nrow = 1, widths = c(1, 1))
dev.off()


## plot tdAdj and bfAdj in one axis
df.swbd.part = df.swbd[, .(inTopicID, role, tdAdj, bfAdj)]
df.swbd.melt = melt(df.swbd.part, id.vars = c('inTopicID', 'role'), measure.vars = c('tdAdj', 'bfAdj'))

setkey(df.swbd.melt, role, variable)
df.swbd.melt$group = 'NTD: follower'
df.swbd.melt[role == 'leader' & variable == 'tdAdj', group := 'NTD: leader']
df.swbd.melt[role == 'follower' & variable == 'bfAdj', group := 'NBF: follower']
df.swbd.melt[role == 'leader' & variable == 'bfAdj', group := 'NBF: leader']

df.swbd.melt$group = factor(df.swbd.melt$group, levels = c('NTD: leader', 'NTD: follower', 'NBF: leader', 'NBF: follower'))

p.swbd.adj = ggplot(subset(df.swbd.melt, inTopicID <= 10), aes(x = inTopicID, y = value)) +
    stat_summary(fun.data = mean_cl_boot, geom = 'ribbon', alpha = .5, aes(fill = group)) +
    stat_summary(fun.y = mean, geom = 'line', aes(lty = group)) +
    stat_summary(fun.y = mean, geom = 'point', aes(shape = group)) +
    scale_x_continuous(breaks = 1:10) +
    theme(legend.position = c(.8, .15)) +
    xlab('within-topic position of sentence') + ylab('metric') +
    guides(fill = guide_legend(title = 'group'),
        lty = guide_legend(title = 'group'),
        shape = guide_legend(title = 'group')) +
    scale_fill_manual(values = c('NTD: leader' = "#E69F00", 'NTD: follower' = "#E69F00",
        'NBF: leader' = "#56B4E9", 'NBF: follower' = "#56B4E9")) +
    scale_linetype_manual(values = c('NTD: leader' = 1, 'NTD: follower' = 3, 'NBF: leader' = 1, 'NBF: follower' = 3)) +
    scale_shape_manual(values = c('NTD: leader' = 1, 'NTD: follower' = 1, 'NBF: leader' = 4, 'NBF: follower' = 4))

pdf('swbd_tdAdj_bfAdj_one.pdf', 5, 5)
plot(p.swbd.adj)
dev.off()

p.swbd.adj1 = ggplot(subset(df.swbd.melt, inTopicID <= 10), aes(x = inTopicID, y = value)) +
    stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width = .2, aes(lty = group)) +
    stat_summary(fun.y = mean, geom = 'line', aes(lty = group)) +
    stat_summary(fun.y = mean, geom = 'point', aes(shape = group)) +
    scale_x_continuous(breaks = 1:10) +
    theme(legend.position = c(.8, .15)) +
    xlab('within-topic position of sentence') + ylab('metric') +
    guides(lty = guide_legend(title = 'group'),
        shape = guide_legend(title = 'group')) +
    scale_linetype_manual(values = c('NTD: leader' = 1, 'NTD: follower' = 3, 'NBF: leader' = 1, 'NBF: follower' = 3)) +
    scale_shape_manual(values = c('NTD: leader' = 1, 'NTD: follower' = 1, 'NBF: leader' = 4, 'NBF: follower' = 4))

pdf('swbd_tdAdj_bfAdj_two.pdf', 5, 5)
plot(p.swbd.adj1)
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
pdf('bnc-entAdj_vs_inTopicID_roles.pdf', 5, 5)
plot(p.bnc2)
dev.off()

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
pdf('bnc-td_vs_inTopicID_roles.pdf')
plot(p.bnc4)
dev.off()

# bf vs inTopicID
p.bnc5 = ggplot(subset(df.bnc, inTopicID <= 10), aes(x = inTopicID, y = bf)) +
    stat_summary(fun.data = mean_cl_boot, geom = 'ribbon', alpha = .5, aes(fill = role)) +
    stat_summary(fun.y = mean, geom = 'line', aes(lty = role)) +
    scale_x_continuous(breaks = 1:10) +
    theme(legend.position = c(.8, .1)) +
    xlab('within-topic position of sentence') + ylab('branching factor')
pdf('bnc-bf_vs_inTopicID_roles.pdf')
plot(p.bnc5)
dev.off()

# tdAdj vs inTopicID
p.bnc6 = ggplot(subset(df.bnc, inTopicID <= 10 & topicID > 1), aes(x = inTopicID, y = tdAdj)) +
    stat_summary(fun.data = mean_cl_boot, geom = 'ribbon', alpha = .5, aes(fill = role)) +
    stat_summary(fun.y = mean, geom = 'line', aes(lty = role)) +
    scale_x_continuous(breaks = 1:10) +
    theme(legend.position = c(.8, .1)) +
    xlab('within-topic position of sentence') + ylab('adjusted tree depth')
pdf('bnc-tdAdj_vs_inTopicID_roles.pdf')
plot(p.bnc6)
dev.off()

# bfAdj vs inTopicID
p.bnc7 = ggplot(subset(df.bnc, inTopicID <= 10 & topicID > 1), aes(x = inTopicID, y = bfAdj)) +
    stat_summary(fun.data = mean_cl_boot, geom = 'ribbon', alpha = .5, aes(fill = role)) +
    stat_summary(fun.y = mean, geom = 'line', aes(lty = role)) +
    scale_x_continuous(breaks = 1:10) +
    theme(legend.position = c(.8, .1)) +
    xlab('within-topic position of sentence') + ylab('adjusted branching factor')
pdf('bnc-bfAdj_vs_inTopicID_roles.pdf')
plot(p.bnc7)
dev.off()





## try plotting swbd and bnc together in one figure
df.swbd.tmp = df.swbd[, .(ent, entc, wordNum, td, bf, tdAdj, bfAdj, inTopicID, role)]
df.swbd.tmp$corpus = 'Switchboard'
df.swbd.tmp$group = 'Switchboard: leader'
df.swbd.tmp[df.swbd.tmp$role == 'follower',]$group = 'Switchboard: follower'

df.bnc.tmp = df.bnc[, .(ent, entc, wordNum, td, bf, tdAdj, bfAdj, inTopicID, role)]
df.bnc.tmp$corpus = 'BNC'
df.bnc.tmp$group = 'BNC: leader'
df.bnc.tmp[df.bnc.tmp$role == 'follower',]$group = 'BNC: follower'

df.all = rbind(df.swbd.tmp, df.bnc.tmp)

# get ggplot default colors
gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}
my_colors = gg_color_hue(2)

# entropy vs inTopicID
p.together1 = ggplot(subset(df.all, inTopicID <= 10), aes(x = inTopicID, y = ent)) +
    stat_summary(fun.data = mean_cl_boot, geom = 'ribbon', alpha = .5, aes(fill = group)) +
    stat_summary(fun.y = mean, geom = 'line', aes(lty = group)) +
    stat_summary(fun.y = mean, geom = 'point', aes(shape = group)) +
    scale_x_continuous(breaks = 1:10) +
    theme(legend.position = c(.8, .15)) +
    xlab('Within-topic position of sentence') + ylab('Per-word entropy') +
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
p.together2 = ggplot(subset(df.all, inTopicID <= 10), aes(x = inTopicID, y = td)) +
    stat_summary(fun.data = mean_cl_boot, geom = 'ribbon', alpha = .5, aes(fill = group)) +
    stat_summary(fun.y = mean, geom = 'line', aes(lty = group)) +
    stat_summary(fun.y = mean, geom = 'point', aes(shape = group)) +
    scale_x_continuous(breaks = 1:10) +
    theme(legend.position = c(.8, .15)) +
    xlab('Within-topic position of sentence') + ylab('Tree depth') +
    guides(fill = guide_legend(title = 'group'),
        lty = guide_legend(title = 'group'),
        shape = guide_legend(title = 'group')) +
    scale_fill_manual(values = c('BNC: leader' = my_colors[2], 'BNC: follower' = my_colors[2],
        'Switchboard: leader' = my_colors[1], 'Switchboard: follower' = my_colors[1])) +
    scale_linetype_manual(values = c('BNC: leader' = 1, 'BNC: follower' = 3, 'Switchboard: leader' = 1, 'Switchboard: follower' = 3)) +
    scale_shape_manual(values = c('BNC: leader' = 1, 'BNC: follower' = 1, 'Switchboard: leader' = 4, 'Switchboard: follower' = 4))
# pdf('all-td_vs_inTopicID_roles.pdf', 5, 5)
# plot(p.together2)
# dev.off()

# branching factor vs inTopicID
p.together3 = ggplot(subset(df.all, inTopicID <= 10), aes(x = inTopicID, y = bf)) +
    stat_summary(fun.data = mean_cl_boot, geom = 'ribbon', alpha = .5, aes(fill = group)) +
    stat_summary(fun.y = mean, geom = 'line', aes(lty = group)) +
    stat_summary(fun.y = mean, geom = 'point', aes(shape = group)) +
    scale_x_continuous(breaks = 1:10) +
    theme(legend.position = c(.8, .15)) +
    xlab('Within-topic position of sentence') + ylab('Branching factor') +
    guides(fill = guide_legend(title = 'group'),
        lty = guide_legend(title = 'group'),
        shape = guide_legend(title = 'group')) +
    scale_fill_manual(values = c('BNC: leader' = my_colors[2], 'BNC: follower' = my_colors[2],
        'Switchboard: leader' = my_colors[1], 'Switchboard: follower' = my_colors[1])) +
    scale_linetype_manual(values = c('BNC: leader' = 1, 'BNC: follower' = 3, 'Switchboard: leader' = 1, 'Switchboard: follower' = 3)) +
    scale_shape_manual(values = c('BNC: leader' = 1, 'BNC: follower' = 1, 'Switchboard: leader' = 4, 'Switchboard: follower' = 4))
# pdf('all-bf_vs_inTopicID_roles.pdf', 5, 5)
# plot(p.together3)
# dev.off()

# wordNum vs inTopicID
p.together4 = ggplot(subset(df.all, inTopicID <= 10), aes(x = inTopicID, y = wordNum)) +
    stat_summary(fun.data = mean_cl_boot, geom = 'ribbon', alpha = .5, aes(fill = group)) +
    stat_summary(fun.y = mean, geom = 'line', aes(lty = group)) +
    stat_summary(fun.y = mean, geom = 'point', aes(shape = group)) +
    scale_x_continuous(breaks = 1:10) +
    theme(legend.position = c(.8, .15)) +
    xlab('Within-topic position of sentence') + ylab('Sentence length (number of words)') +
    guides(fill = guide_legend(title = 'group'),
        lty = guide_legend(title = 'group'),
        shape = guide_legend(title = 'group')) +
    scale_fill_manual(values = c('BNC: leader' = my_colors[2], 'BNC: follower' = my_colors[2],
        'Switchboard: leader' = my_colors[1], 'Switchboard: follower' = my_colors[1])) +
    scale_linetype_manual(values = c('BNC: leader' = 1, 'BNC: follower' = 3, 'Switchboard: leader' = 1, 'Switchboard: follower' = 3)) +
    scale_shape_manual(values = c('BNC: leader' = 1, 'BNC: follower' = 1, 'Switchboard: leader' = 4, 'Switchboard: follower' = 4))
# pdf('all-wordNum_vs_inTopicID_roles.pdf', 5, 5)
# plot(p.together4)
# dev.off()



### plot on one canvas
library(gridExtra)

# the function that gets the legend of a plot (for multiple plotting that shares one legend)
g_legend = function(p) {
    tmp = ggplotGrob(p)
    leg = which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend = tmp$grobs[[leg]]
    legend
}

p.td = p.together2 + theme(legend.position = 'bottom', plot.title = element_text(size = 12))
p.bf = p.together3
p.sl = p.together4

lgd = g_legend(p.td)

pdf('sl_td_bf.pdf', 10, 4)
grid.arrange(arrangeGrob(p.sl + theme(legend.position = 'none'),
                        p.td + theme(legend.position = 'none'),
                        p.bf + theme(legend.position = 'none'), ncol = 3),
            lgd, nrow = 2, heights = c(9, 1))
dev.off()
