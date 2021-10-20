# Code for reproducing manuscript figures and tables

# Set working directory
setwd("~/dropbox/research/mystudies/ltwstudy_2017/es_specialissue/managementcostpaper/Github/FigureCodeandData/") # Set the working directory to 

# Import and process management cost results data
management_results_r1 <- read.csv("emds_summaries.csv") # round 1
management_results_r2 <- read.csv("emds_summaries_130reps.csv") # round 2

management_results_r1 <- management_results_r1 %>% 
  mutate(round = "1") %>%   
  mutate(decade_num = 10*rep(1:10, 4)) %>% 
  mutate(climate.gcm = "CanESM",
         rcp= 4.5) %>% 
  mutate(biomass.cut.mtons_mean = NA, 
         biomass.cut.mtons_sd = NA) %>% 
  dplyr::select(X, round, scenario, climate.gcm, rcp, decade_num, decade:timber.cuft.annualavg_sd, biomass.cut.mtons_mean, biomass.cut.mtons_sd, cumulative.sequestration.allpools.mtons.carbon.decadeavg_mean, cumulative.sequestration.allpools.mtons.carbon.decadeavg_sd)

management_results_r2 <- management_results_r2 %>% 
  mutate(round = ifelse(scenario ==5, "1", "2")) %>%  # scenario 5 belongs in round 1 
  mutate(decade_num = 10*rep(1:10, 41)) %>% 
  dplyr::select(X, round, scenario, climate.gcm, rcp, decade_num, decade:cumulative.sequestration.allpools.mtons.carbon.decadeavg_sd)

management_results <- rbind(management_results_r1, management_results_r2)

## Filter to data frame for management cost analysis 
management_results <- management_results %>% 
  filter(round == 1)

# Figure 4
Fig4Data <- read.csv("/FigureCodeandData/Figure4Data.csv")

Fig4 <- Fig4Data %>% 
  ggplot() +
  geom_line(aes(x=DBH, HarvestPct)) +
  xlab("Diameter at breast height (inches)") + ylab("Percent Biomass Harvested") +
  ylim(0,100) +
  theme_bw()

# Figure 5
Fig5Input <- management_results %>% 
  filter(decade_num <= 30) %>% 
  group_by(scenario) %>% 
  summarize(TotalTreatmentCost = mean(manage.cost.rxfire.timberrev.transport.net_mean)/1000,
            TotalManagementCost = mean(all.costs.manage.timber.transport.rx.wildfire_mean)/1000,
            WildfireSuppressionCost = mean(wildfire.cost_mean)/1000,
            NetRevenue = mean(timber.biomass.rev.post.trans.net_mean)/1000) %>%
  mutate(TotalTreatmentCost = TotalTreatmentCost + NetRevenue) %>% 
  mutate(RxCost = ifelse(scenario==4, 550*750/1000,
                         ifelse(scenario==5, 2600*750/1000, 0))) %>% 
  mutate(ThinningCost = TotalTreatmentCost - RxCost) %>% 
  mutate(Scenario = ifelse(scenario == 1, "S1",
                           ifelse(scenario == 2, "S2",
                                  ifelse(scenario == 3, "S3",
                                         ifelse(scenario == 4, "S4",
                                                ifelse(scenario == 5, "S5",NA)))))) %>% 
  mutate(NetRevenue = -NetRevenue) %>% 
  dplyr::select(-scenario)

ResultsFig1 <- Fig5Input %>% 
  dplyr::select(-TotalTreatmentCost) %>%
  gather(key=CostCategory, value=Cost, -Scenario) %>% 
  mutate(CostCategory = factor(CostCategory, levels=c("ThinningCost", "NetRevenue", "RxCost", "WildfireSuppressionCost", "TotalManagementCost"))) %>% 
  ggplot() +
  geom_bar(aes(CostCategory, Cost, fill=CostCategory), stat="identity", position=position_dodge()) +
  facet_wrap(~Scenario, nrow=1) +
  scale_fill_manual(name = "Category", labels=c("Thinning", "Timber/Biomass Sale Subsidy", "Rx Fire", "Wildfire Suppression", "Total"),
                    values=c("#999999", "#E69F00", "#56B4E9", "#009E73","#D55E00")) + 
  ylab("Average Annual Cost (1,000$)") + xlab("") + 
  theme_classic() +
  theme(axis.text.x=element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_rect(color = "grey", fill = NA, size = 0.5))

# Table 4
# First 10 years
management_results %>% 
  filter(decade_num == 10) %>% 
  dplyr::select(wildfire.cost_mean, manage.cost.rxfire.timberrev.transport.net_mean, timber.biomass.rev.post.trans.net_mean)

# First 30 years
management_results %>% 
  filter(decade_num <= 30) %>%
  group_by(scenario) %>% 
  summarize(wildfire.cost_mean = mean(wildfire.cost_mean),
            manage.cost.rxfire.timberrev.transport.net_mean = mean(manage.cost.rxfire.timberrev.transport.net_mean),
            timber.biomass.rev.post.trans.net_mean = mean(timber.biomass.rev.post.trans.net_mean)) %>% 
  dplyr::select(scenario, wildfire.cost_mean, manage.cost.rxfire.timberrev.transport.net_mean, timber.biomass.rev.post.trans.net_mean)

1653+2344-130
# over 100 years
management_results %>% 
  group_by(scenario) %>% 
  summarize(wildfire.cost_mean = mean(wildfire.cost_mean),
            manage.cost.rxfire.timberrev.transport.net_mean = mean(manage.cost.rxfire.timberrev.transport.net_mean),
            timber.biomass.rev.post.trans.net_mean = mean(timber.biomass.rev.post.trans.net_mean)) %>% 
  select(scenario, wildfire.cost_mean, manage.cost.rxfire.timberrev.transport.net_mean, timber.biomass.rev.post.trans.net_mean)

