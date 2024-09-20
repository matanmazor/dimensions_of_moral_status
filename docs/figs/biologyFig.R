p <- exp1_df %>%
  filter(dimension=='biology') %>%
  dplyr::select(c('subj_id','chain_number','mwj','conscNeg', 'conscPos')) %>%
  gather('species','consciousness',conscNeg,conscPos)%>%
  group_by(mwj,species) %>%
  summarise(consc = mean(consciousness),
            sem=se(consciousness))%>%
  mutate(species=factor(species, levels=c('conscPos','conscNeg'), labels=c('pos','neg')),
         mwj=factor(mwj,levels=c('pos','neg'), labels=c('non-human-like','human-like'))) %>%
  ggplot(aes(x=mwj,fill=species, y=consc))  + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=consc-sem, ymax=consc+sem), width=.2,
                position=position_dodge(.9))+labs( x="Saved species", y = "Consciousness rating")+
  theme_classic() +theme( plot.margin=margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), legend.pos='na')+
  scale_fill_manual(values=c('#999999','#FFFFFF'))  +
  coord_cartesian(ylim = c(50, NA)) 

ggsave('biology1.png',p, width=3, height=4);

p <- exp2_df %>%
  filter(dimension=='DNA') %>%
  dplyr::select(c('subj_id','chain_number','mwj','conscNeg', 'conscPos')) %>%
  gather('species','consciousness',conscNeg,conscPos)%>%
  group_by(mwj,species) %>%
  summarise(consc = mean(consciousness),
            sem=se(consciousness))%>%
  mutate(species=factor(species, levels=c('conscPos','conscNeg'), labels=c('pos','neg')),
         mwj=factor(mwj,levels=c('pos','neg'), labels=c('non-human-like','human-like'))) %>%
  ggplot(aes(x=mwj,fill=species, y=consc))  + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=consc-sem, ymax=consc+sem), width=.2,
                position=position_dodge(.9))+labs(x="Saved species", y = "Consciousness rating")+
  theme_classic() +theme( plot.margin=margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), legend.pos='na')+
  scale_fill_manual(values=c('#999999','#FFFFFF'))  +
  coord_cartesian(ylim = c(50, NA)) 

ggsave('DNAcorrelation2.png',p, width=3, height=4);

p <- exp2_df %>%
  filter(dimension=='blood') %>%
  dplyr::select(c('subj_id','chain_number','mwj','conscNeg', 'conscPos')) %>%
  gather('species','consciousness',conscNeg,conscPos)%>%
  group_by(mwj,species) %>%
  summarise(consc = mean(consciousness),
            sem=se(consciousness))%>%
  mutate(species=factor(species, levels=c('conscPos','conscNeg'), labels=c('pos','neg')),
         mwj=factor(mwj,levels=c('pos','neg'), labels=c('non-human-like','human-like'))) %>%
  ggplot(aes(x=mwj,fill=species, y=consc))  + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=consc-sem, ymax=consc+sem), width=.2,
                position=position_dodge(.9))+labs(x="Saved species", y = "Consciousness rating")+
  theme_classic() +theme( plot.margin=margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), legend.pos='na')+
  scale_fill_manual(values=c('#999999','#FFFFFF'))  +
  coord_cartesian(ylim = c(50, NA)) 


ggsave('bloodcorrelation2.png',p, width=3, height=4);

p <- exp3_df %>%
  filter(dimension=='DNA') %>%
  dplyr::select(c('subj_id','chain_number','mwj','conscNeg', 'conscPos')) %>%
  gather('species','consciousness',conscNeg,conscPos)%>%
  group_by(mwj,species) %>%
  summarise(consc = mean(consciousness),
            sem=se(consciousness))%>%
  mutate(species=factor(species, levels=c('conscPos','conscNeg'), labels=c('pos','neg')),
         mwj=factor(mwj,levels=c('pos','neg'), labels=c('non-human-like','human-like'))) %>%
  ggplot(aes(x=mwj,fill=species, y=consc))  + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=consc-sem, ymax=consc+sem), width=.2,
                position=position_dodge(.9))+labs( x="Saved species", y = "Consciousness rating")+
  theme_classic() +theme( plot.margin=margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), legend.pos='na')+
  scale_fill_manual(values=c('#999999','#FFFFFF'))  +
  coord_cartesian(ylim = c(50, NA)) 

ggsave('DNAcorrelation3.png',p, width=3, height=4);

p <- exp3_df %>%
  filter(dimension=='blood') %>%
  dplyr::select(c('subj_id','chain_number','mwj','conscNeg', 'conscPos')) %>%
  gather('species','consciousness',conscNeg,conscPos)%>%
  group_by(mwj,species) %>%
  summarise(consc = mean(consciousness),
            sem=se(consciousness))%>%
  mutate(species=factor(species, levels=c('conscPos','conscNeg'), labels=c('pos','neg')),
         mwj=factor(mwj,levels=c('pos','neg'), labels=c('non-human-like','human-like'))) %>%
  ggplot(aes(x=mwj,fill=species, y=consc))  + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=consc-sem, ymax=consc+sem), width=.2,
                position=position_dodge(.9))+labs(x="Saved species", y = "Consciousness rating")+
  theme_classic() +theme( plot.margin=margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), legend.pos='na')+
  scale_fill_manual(values=c('#999999','#FFFFFF'))  +
  coord_cartesian(ylim = c(50, NA)) 

ggsave('bloodcorrelation3.png',p, width=3, height=4);



t.test(exp3_df %>%
         filter(dimension=='DNA' & mwj=='pos') %>%
         pull('consc_diff'), 
       exp3_df %>%
         filter(dimension=='DNA' & mwj=='neg') %>%
         pull('consc_diff'))