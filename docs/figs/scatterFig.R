colors_exp1 <- c('#DDCB77','#CC6677', '#AA4599','#882155','#FF0000','#12783D','#44AA99','#89CCED')

colors_exp2 <- c('#DDCB77','#DDCB77', '#FF0000','#FF0000','#89CCED','#89CCED','#44AA99','#44AA99')

p1 <- ggplot(exp1.relation_df %>%
               mutate(dimension = factor(dimension, levels=c("pRichness","eRichness","unity","time","self","size","appearance","biology"), labels=c("phenomenal","evaluative","unity", "temporality", "selfhood","size","physical", "biological")))%>%
               mutate(isself = (dimension=='selfhood')),
             aes(x=consc_diff,y=numNeg)) +
  geom_point(aes(fill=dimension),size=3,alpha=0.5, shape=21, color='white')+
  geom_hline(yintercept=10,linetype='dashed')+
  geom_text_repel(aes(label=dimension, color=isself), hjust=0.3, vjust=-0.1)+
  labs(x="Difference in consciousness ratings (%)", 
       y = "Number of feature-negative aliens") +
  theme_classic() +
  theme(legend.position = "none")+
  scale_color_manual(values=c('#000000','#FF0000'))+
  scale_fill_manual(values=colors_exp1)+
  xlim(0,20)

ggsave('scatter1.png',p1, width=4, height=4);

p2 <- ggplot(exp2.relation_df %>%
               mutate(isself = (dimension=='mirror' | 
                                  dimension=='deception'),
                      dimension=factor(dimension, levels=c('discrimination','trace','mirror','deception','DNA','blood','eyes','limbs'),
                                       labels=c('discrimination','trace','mirror','hiding','DNA','blood','eyes','limbs'))),
             aes(x=consc_diff,y=numNeg))  +
  geom_hline(yintercept=10,linetype='dashed')+
  geom_point(aes(fill=dimension),size=3,alpha=0.5, shape=21, color='white')+
  geom_text_repel(aes(label=dimension, color=isself), hjust=0.3, vjust=-0.1)+
  labs(x="Difference in consciousness ratings (%)", 
       y = "Number of feature negative aliens") +
  theme_classic() +
  theme(legend.position = "none")+
  scale_color_manual(values=c('#000000','#FF0000'))+
  scale_fill_manual(values=colors_exp2)+
  xlim(0,20)

ggsave('scatter2.png',p2, width=4, height=4);


p3 <- ggplot(exp3.relation_df %>%
               mutate(isself = (dimension=='mirror' | 
                                  dimension=='deception'),
                      dimension=factor(dimension, levels=c('discrimination','trace','mirror','deception','DNA','blood','eyes','limbs'),
                                       labels=c('discrimination','trace','mirror','hiding','DNA','blood','eyes','limbs'))),
             aes(x=consc_diff,y=numNeg))  +
  geom_hline(yintercept=10,linetype='dashed')+
  geom_text_repel(aes(label=dimension, color=isself), hjust=0.3, vjust=-0.1)+
  geom_point(aes(fill=dimension),size=3,alpha=0.5, shape=21, color='white')+
  labs(x="Difference in consciousness ratings (%)", 
       y = "Number of feature negative aliens") +
  theme_classic() +
  theme(legend.position = "none")+
  scale_color_manual(values=c('#000000','#FF0000'))+
  scale_fill_manual(values=colors_exp2)+
  xlim(0,15) 

ggsave('scatter3.png',p3, width=4, height=4);