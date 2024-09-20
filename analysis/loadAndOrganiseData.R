# %% Experiment 1 %% 

# The first chain of experiment 1 is handled separately because we didn't yet document chain number, 
# and we had syncing problems with two participants that are now excluded. 

exp1.chain1 <- list();

exp1.chain1$list <- read_file('../experiments/Exp1/data/jatos_results_chain1.txt') %>%
  str_split("Consent given.\nConsent given.\n")

exp1.chain1$df <- data.frame();

for (i in 2:length(exp1.chain1$list[[1]])) {
  
  this_subject <- read.csv(text=exp1.chain1$list[[1]][i]) %>%
    dplyr::select(c('PROLIFIC_PID',
                    'participant_number',
                    'mwj',
                    'conscPos',
                    'conscNeg',
                    'dimension',
                    'numNeg',
                    'description',
                    'responses',
                    'trial_type'));
  
  exp1.chain1$df <- rbind(exp1.chain1$df, this_subject);
}

# Excluded due to a technical issue in data collection
excluded <- c('5fedc8ece6b28104f883f096','5d4ad488dfd8230018fd0b45');

exp1.chain1$df.trials <- exp1.chain1$df %>%
  filter(trial_type=='html-moral-worth-slides-trial' & !(PROLIFIC_PID %in%excluded)) %>%
  dplyr::select(c('PROLIFIC_PID',
                  'participant_number',
                  'mwj',
                  'conscPos',
                  'conscNeg',
                  'dimension',
                  'numNeg',
                  'description'))%>%
  rename(subj_id = PROLIFIC_PID)%>%
  mutate(description=str_squish(description))%>%
  arrange('participant_number')%>%
  mutate(participant_number=ifelse((subj_id=='5fa97a5192918912cb3d8173' | subj_id=='5e2d94d8ed8c8a3887580acb'),
                                   participant_number,participant_number-1),
         subj_id=participant_number);

exp1.chain1$df.trials$chain_number=1;
exp1.chain1$df.trials%>%relocate('chain_number');

write.csv(exp1.chain1$df.trials,'../experiments/Exp1/data/jatos_results_chain1.csv')


load_chain <- function(exp_number, chain_number) {
  
  chain <-list()
  
  chain$list <- read_file(
    sprintf('../experiments/Exp%d/data/jatos_results_chain%d.txt',
            exp_number, chain_number)) %>%
    str_split("Consent given.\nConsent given.\n")
  
  chain$df <- data.frame();
  
  for (i in 2:length(chain$list[[1]])) {
    chain$df <- rbind(chain$df, read.csv(text=chain$list[[1]][i]))
  }
  
  chain$df.trials <- chain$df %>%
    filter(str_detect(trial_type,'^html-moral-worth')) %>%
    dplyr::select(c('chain_number',
                    'PROLIFIC_PID',
                    'participant_number',
                    'mwj',
                    'conscPos',
                    'conscNeg',
                    'dimension',
                    'numNeg',
                    'description'))%>%
    rename('subj_id' = 'PROLIFIC_PID')%>%
    mutate(description=str_squish(description))%>%
    arrange('participant_number')
  
  chain$df.trials$chain_number = chain_number;
  
  write.csv(chain$df.trials,
            sprintf('../experiments/Exp%d/data/jatos_results_chain%d.csv',exp_number,chain_number))
  
  return(chain)
}

exp1.chain2 <- load_chain(1,2);
exp1.chain3 <- load_chain(1,3);
exp1.chain4 <- load_chain(1,4);
exp1.chain5 <- load_chain(1,5);

exp1_df <- rbind(exp1.chain1$df.trials, 
                 exp1.chain2$df.trials, 
                 exp1.chain3$df.trials,
                 exp1.chain4$df.trials,
                 exp1.chain5$df.trials)%>%
  mutate(consc_diff = conscPos-conscNeg,
         dimension=factor(dimension, levels=c(
           'size',
           'appearance',
           'biology',
           'unity',
           'pRichness',
           'eRichness',
           'time',
           'self')));

write.csv(exp1_df,'../experiments/Exp1/data/all_data.csv')

# %% Experiment 2 %% 

exp2.chain1 <- load_chain(2,1);
exp2.chain2 <- load_chain(2,2);

exp2_df <- rbind(exp2.chain1$df.trials, 
                 exp2.chain2$df.trials)%>%
  mutate(consc_diff = conscPos-conscNeg,
         dimension=factor(dimension, levels=c(
           'limbs',
           'blood',
           'eyes',
           'DNA',
           'deception',
           'trace',
           'discrimination',
           'mirror')));

write.csv(exp2_df,'../experiments/Exp2/data/all_data.csv')

# %% Experiment 3

exp3.chain1 <- load_chain(3,1);
exp3.chain2 <- load_chain(3,2);
exp3.chain3 <- load_chain(3,3);

exp3_df <- rbind(exp3.chain1$df.trials, 
                 exp3.chain2$df.trials,
                 exp3.chain3$df.trials)%>%
  mutate(consc_diff = conscPos-conscNeg,
         dimension=factor(dimension, levels=c(
           'eyes',
           'limbs',
           'DNA',
           'blood',
           'discrimination',
           'trace',
           'mirror',
           'deception')));

write.csv(exp3_df,'../experiments/Exp3/data/all_data.csv')
