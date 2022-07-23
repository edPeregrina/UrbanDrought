
#Import Libraries
library(ggstatsplot)
library(tidyverse)
library(ggpubr)
library(coin)
library(reshape2)
library(lme4)
library(lubridate)
library(cowplot)
library(ggplot2)
library(datarium)
library(rstatix)

#Import Data
wgn = read.csv("C:/Users/peregrin/thesis_models/UrbanWaterGap/wgn.csv")
stor = read.csv("C:/Users/peregrin/thesis_models/UrbanWaterGap/stor.csv")
inc = read.csv("C:/Users/peregrin/thesis_models/UrbanWaterGap/incidents_dnd.csv")
areas = read.csv("C:/Users/peregrin/thesis_models/UrbanWaterGap/areas_id.csv")

stor$date_m<-as.Date(stor$date,"%d/%m/%Y")
format(stor$date_m, "%m/%Y")
wgn$date<-as.Date(wgn$date,"%Y-%m-%d")
format(wgn$date, "%m/%Y")
inc$date<-as.Date(paste0(as.character(inc$date), '-01'), format='%Y-%m-%d')
format(inc$date, "%m/%Y")
inc$Type<-"Incident"

inc.date<-inc$date
inc.fact<-as.factor(inc$X8675)
norm.wg<-wgn$X8675
stor.tot<-stor$X8675

df_all1<-data.frame(inc$date,inc.fact,norm.wg)
df_all2<-data.frame(inc$date,inc.fact,stor.tot)

head(df_all1)
df1_long <- melt(df_all1, id.vars=c("inc.date","inc.fact","norm.wg"))
df2_long <- melt(df_all2, id.vars=c("inc.date","inc.fact","stor.tot"))
str(df1_long)

stat.test <- df1_long %>% 
  wilcox_test(norm.wg ~ inc.fact) %>%
  add_significance()
stat.test

df1_long %>% wilcox_effsize(norm.wg ~ inc.fact)



#-----------For loop

cities <- c(8675, 512, 3268,1303)

#///////////////LOOP START////////////////////
count <- 0
plt_keep <- matrix(ncol=1, nrow=length(cities))
stat_wg_keep <- matrix(ncol=1, nrow=length(cities))
stat_s_keep <- matrix(ncol=1, nrow=length(cities))
for (val in cities) {
count<-count+1
fid=val
#find corresponding name for reporting
print(fid)
#PART 0
inc_call<-gsub(" ", "",paste("inc$X",fid))
wgn_call<-gsub(" ", "",paste("wgn$X",fid))
stor_call<-gsub(" ", "",paste("stor$X",fid))
area_call<-eval(parse(text=(gsub(" ", "",paste("areas$X",fid)))))

#PART 1
inc_data<-eval(parse(text = inc_call))
inc.fact<-as.factor(inc_data)
norm.wg<-eval(parse(text = wgn_call))
stor.tot<-eval(parse(text = stor_call))

length(levels(inc.fact))

if (length(levels(inc.fact)) == 1){
    print ("City has no drought incidents recorded")

} else {

#PART 2
df_all1<-data.frame(inc$date,inc.fact,norm.wg,stor.tot)
df_alls<-data.frame(inc$date,inc.fact,stor.tot)
df_allw<-data.frame(inc$date,inc.fact,norm.wg)

df1_long <- melt(df_all1, id.vars=c("inc.date","inc.fact","norm.wg","stor.tot"))

df1_long

df1_longs <- melt(df_alls, id.vars=c("inc.date","inc.fact","stor.tot"))
df1_longw <- melt(df_allw, id.vars=c("inc.date","inc.fact","norm.wg"))



stat.tests <-   wilcox_test(stor.tot ~ inc.fact, alternative = c("two.sided"), detailed=TRUE) %>%
  add_significance()
print(stat.tests)
ef_sizes<-df1_longs %>% wilcox_effsize(stor.tot ~ inc.fact)
print(ef_sizes)

stat.testn <-   wilcox_test(norm.wg ~ inc.fact, alternative = c("two.sided"), detailed=TRUE) %>%
  add_significance()
print(stat.testn)
ef_sizen<-df1_longw %>% wilcox_effsize(norm.wg ~ inc.fact)
print(ef_sizen)


sample_size = df1_longs %>% group_by(inc.fact) %>% summarize(num=n())

plt1<-df1_longs %>%
  left_join(sample_size) %>%
  mutate(myaxis = paste0(inc.fact, "\n", "n=", num)) %>%
  ggplot( aes(x=myaxis, y=stor.tot, fill=inc.fact))+
    geom_violin(width=1.0, trim=FALSE) +
    geom_boxplot(width=0.1, color="black", fill="grey")+
    theme(
      legend.position="none",
      plot.title=element_text(size=18, face="bold"),
      text=element_text(size=14))+
      ggtitle(paste("Storage -",area_call))+

    xlab("")+
    ylab("Storage [m]")+
    scale_fill_manual(values=c("#E69F00", "#56B4E9"))

plt2<-df1_longw %>%
  left_join(sample_size) %>%
  mutate(myaxis = paste0(inc.fact, "\n", "n=", num)) %>%
  ggplot( aes(x=myaxis, y=norm.wg, fill=inc.fact))+
  geom_violin(width=1.0, trim=TRUE) +
  geom_boxplot(width=0.1, color="black", fill="grey")+
  theme(
    legend.position="none",
    plot.title=element_text(size=18, face="bold"),
    text=element_text(size=14))+
    ggtitle(paste("Water Gap -",area_call))+

  xlab("")+
  ylab("Relative Water Gap [-]")+
  scale_fill_manual(values=c("#E69F00", "#56B4E9"))


plot_grid(plt1,plt2,
	labels = "",
	ncol=2)
# 
 target<-paste0('massplot/',as.character(val), 'dist_plot', '.svg')
 ggsave(target)

}
}


