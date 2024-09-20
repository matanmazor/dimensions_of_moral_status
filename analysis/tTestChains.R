# extract ESS
exp2.ESS <- exp2.mwj_df %>%
  group_by(dimension,chain_number) %>%
  summarise(ESS=ESS(numNeg)) %>%
  group_by(dimension) %>%
  summarise(ESS=sum(ESS))

exp3.ESS <- exp3.mwj_df %>%
  group_by(dimension,chain_number) %>%
  summarise(ESS=ESS(numNeg)) %>%
  group_by(dimension) %>%
  summarise(ESS=sum(ESS))

# extract mean and SD
exp2.numNeg <- exp2.mwj_df %>%
  group_by(dimension) %>%
  summarise(mean=mean(numNeg)-10,
            sd=sd(numNeg)) %>%
  merge(exp2.ESS)%>%
  mutate(se=sd/sqrt(ESS),
         t=mean/se,
         p = pt(t,df=ESS-1,lower.tail=F))

exp3.numNeg <- exp3.mwj_df %>%
  group_by(dimension) %>%
  summarise(mean=mean(numNeg)-10,
            sd=sd(numNeg)) %>%
  merge(exp3.ESS)%>%
  mutate(se=sd/sqrt(ESS),
         t=mean/se,
         p = pt(t,df=ESS-1,lower.tail=F))