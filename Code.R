# 31 August 2021—User David Mason—Simulating heterocarpochory
# Setting up the workspace ####
library(tidyverse)
options(scipen = 999)
set.seed(901)
# Simulations ####

# Wind only simulation
wind <- rlogis(n=10000,location=0.1123,scale=2.3616)
												
n <- 10000
p.wind <- c(0.99, 0.95, 0.90, 0.80, 0.70)

# 1% heterocarpochory
mix.sims.out.1 <- rep(0,n)
for(i in 1:n){
  
  U <- runif(n=1)
  if(U <= p.wind[1]){
    #pick wind kernel:
    mix.sims.out.1[i] <- rlogis(n=1,
															location=0.1123,scale=2.3616)
  }else{
    #pick animal kernel: 
    mix.sims.out.1[i] <- rlogis(n=1,location=222.75,scale=2.1019)
  }
}

# 5% heterocarpochory
mix.sims.out.5 <- rep(0,n)
for(i in 1:n){
  
  U <- runif(n=1)
  if(U <= p.wind[2]){
    #pick wind kernel:
    mix.sims.out.5[i] <- rlogis(n=1,
															location=0.1123,scale=2.3616)
  }else{
    #pick animal kernel: 
    mix.sims.out.5[i] <- rlogis(n=1,location=222.75,scale=2.1019)
  }
}

# 10% heterocarpochory
mix.sims.out.10 <- rep(0,n)
for(i in 1:n){
  
  U <- runif(n=1)
  if(U <= p.wind[3]){
    #pick wind kernel:
    mix.sims.out.10[i] <- rlogis(n=1,
															location=0.1123,scale=2.3616)
  }else{
    #pick animal kernel: 
    mix.sims.out.10[i] <- rlogis(n=1,location=222.75,scale=2.1019)
  }
}

# 20% heterocarpochory
mix.sims.out.20 <- rep(0,n)
for(i in 1:n){
  
  U <- runif(n=1)
  if(U <= p.wind[4]){
    #pick wind kernel:
    mix.sims.out.20[i] <- rlogis(n=1,
															location=0.1123,scale=2.3616)
  }else{
    #pick animal kernel: 
    mix.sims.out.20[i] <- rlogis(n=1,location=222.75,scale=2.1019)
  }
}

# 30% heterocarpochory
mix.sims.out.30 <- rep(0,n)
for(i in 1:n){
  
  U <- runif(n=1)
  if(U <= p.wind[5]){
    #pick wind kernel:
    mix.sims.out.30[i] <- rlogis(n=1,
															location=0.1123,scale=2.3616)
  }else{
    #pick animal kernel: 
    mix.sims.out.30[i] <- rlogis(n=1,location=222.75,scale=2.1019)
  }
}
# Convert the simulations to dataframes ####
wind <- as.data.frame(wind)
mix.sims.out.1 <- as.data.frame(mix.sims.out.1)
mix.sims.out.5 <- as.data.frame(mix.sims.out.5)
mix.sims.out.10 <- as.data.frame(mix.sims.out.10)
mix.sims.out.20 <- as.data.frame(mix.sims.out.20)
mix.sims.out.30 <- as.data.frame(mix.sims.out.30)

# Combine the data frames into a list
df.list <- list(wind,mix.sims.out.1,mix.sims.out.5,
								mix.sims.out.10,mix.sims.out.20,mix.sims.out.30)

# Give the distance column a name #
library(plyr)
df.list <- llply(df.list, function(x) {
                colnames(x) <- c("Distance")
                return(x)
                })
# Drop zeros from the simulations ####
df.list <- lapply(df.list, function(x) filter(x, Distance >=0))
# Sample 5000 from the remaining pool ####
df.list <- lapply(df.list, function(x) sample(x$Distance, 5000,replace = FALSE))
# Create dataframe for quantiles ####

# Create a vector of heterocarpochory probabilities
Probability <- c("Wind only", "1", "5", "10", "20", "30")

# Calculate 50th quantile of each simulation
Fifty <- lapply(df.list, median)

# Calculate 95th quantile of each simulation
Ninety.five <- lapply(df.list, quantile, probs=c(.95))

# Combine into dataframe 
quant.df <- as.data.frame(cbind(Probability,Fifty,Ninety.five))

# Make sure the values are numeric
quant.df$Fifty <- as.numeric(quant.df$Fifty)
quant.df$Ninety.five <- as.numeric(quant.df$Ninety.five)

# Add columns for change compared to wind-only and drop wind-only
quant.df.delta <- quant.df %>% 
	mutate(Delta.50 = Fifty-quant.df[1,2],
				 Delta.95 = Ninety.five-quant.df[1,3]) %>% 
	filter(Probability != "Wind only")

# Create the change in percentile figure ####
quant.df.delta_long <- pivot_longer(quant.df.delta, cols = c("Delta.50",
																		 "Delta.95"), names_to = "Parameter")
quant.df.delta_long$Parameter <- plyr::revalue(quant.df.delta_long$Parameter, 
																 c("Delta.50"="50th percentile", 
																 "Delta.95"="95th percentile"))
quant.df.delta_long$Probability <- factor(quant.df.delta_long$Probability, 
								levels = c("1", "5", "10", "20", "30"), ordered = TRUE)


ggplot(quant.df.delta_long, aes(x = Probability, y = value))+
			 		geom_point(size=8)+
					ylab("Increase in dispersal distance (m)")+
					theme_classic()+
					scale_x_discrete()+
					xlab("Probability of Heterocarpochory (%)")+
					theme(text = element_text(size = 25))+
					theme(axis.title.y = element_text(vjust = 2))+
					facet_wrap(~Parameter, scales = "free_y", nrow = 2)+
				  theme(legend.position = "top")

ggsave("qant.png", dpi = 300)

# Add a kernel column to each dataframe and bring back colname ####
Kernels <- c("Wind only", "1%", "5%", "10%", "20%", "30%")
df.list <- mapply(cbind, df.list, "Kernel"= Kernels, SIMPLIFY=F)
df.list <- llply(df.list, function(x) {
                colnames(x) <- c("Distance", "Kernel")
                return(x)
                })

# Extract data frames from list to make figures ####
wind.only <- as.data.frame(df.list[[1]])
Hetero.1 <- as.data.frame(df.list[[2]])
Hetero.10 <- as.data.frame(df.list[[4]])
Hetero.30 <- as.data.frame(df.list[[6]])

comb.kernel <- rbind(Hetero.1,Hetero.10,Hetero.30)

# Create histogram figure ####

# Make Distance numeric
comb.kernel$Distance <- as.numeric(as.character(comb.kernel$Distance))
wind.only$Distance <- as.numeric(as.character(wind.only$Distance))

# Drop the kernel description for wind-only so facet-wrap doesn't
# get tripped up
wind.only <- wind.only[1]

# Filter for values <= 20 for left panel
comb.kernel.lowvalues <- comb.kernel %>% 
	filter(Distance <= 20)
wind.only.lowvalues <- wind.only%>% 
	filter(Distance <= 20)

p2 <- ggplot(comb.kernel.lowvalues, aes(x=Distance,fill=Kernel))+
	geom_histogram(wind.only.lowvalues, mapping = aes(x=Distance), fill='lightgray',
								 bins = 50, alpha = 0.8, color = 'black')+
	geom_histogram(position="identity",
  							 bins = 50, color = "black", alpha = 0.7)+
	scale_fill_manual(values = c('#DEEBF7','#9ECAE1','#3182BD'))+
	xlab("")+
	ylab("Frequency")+
	theme_classic()+
	scale_x_continuous(expand = c(0,0.02))+
	scale_y_continuous(expand = c(0,0),
										 limits = c(0,450))+
	facet_wrap(~Kernel,ncol = 1)+
	theme(strip.background = element_blank(),strip.text.x = element_blank(),
        panel.background = element_rect(colour = "black", size=1.5),
				text = element_text(size=25),panel.spacing = unit(1.5, "lines"),
				legend.position="none", legend.title = element_blank())

# Filter for values >= 100 for right panel
comb.kernel.highvalues <- comb.kernel %>% 
	filter(Distance >= 100)
wind.only.highvalues <- wind.only %>% 
	filter(Distance >= 100)

p3 <- ggplot(comb.kernel.highvalues, aes(x=Distance,fill=Kernel))+
	geom_histogram(wind.only.highvalues, mapping = aes(x=Distance), fill='lightgray',
								 bins = 50, alpha = 0.8, color = 'black')+
	geom_histogram(position="identity",
  							 bins = 50, color = "black", alpha = 0.7)+
	scale_fill_manual(values = c('#DEEBF7','#9ECAE1','#3182BD'))+
	xlab("")+
	ylab("Frequency")+
	theme_classic()+
	scale_x_continuous(expand = c(0,0.02))+
	scale_y_continuous(expand = c(0,0),
										 limits = c(0,450))+
	facet_wrap(~Kernel,ncol = 1)+
	theme(strip.background = element_blank(),strip.text.x = element_blank(),
        panel.background = element_rect(colour = "black", size=1.5),
				text = element_text(size=25),panel.spacing = unit(1.5, "lines"),
				legend.position="none", legend.title = element_blank())+
		theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

# Combine the individual figures ####
library(ggpubr)
library(grid)
combo <- ggarrange(p2, p3, common.legend = TRUE, legend = "top")
ggsave("combo.png", dpi = 300)

# Determine how much data is missed by the broken axis ####
comb.kernel %>% 
	filter(Distance > 20 & Distance <=200) # 5 observations

wind.only %>% filter(Distance > 20 & Distance <=200) # 1 observations

6/20000
# 0.0003

# Calculate number of long distance dispersal events ####
wind.only <- as.data.frame(df.list[[1]])
wind.only$Distance <- as.numeric(as.character(wind.only$Distance))
max(wind.only$Distance) # 23.11
length(which(wind.only$Distance>=100)) # 0

Hetero.1 <- as.data.frame(df.list[[2]])
Hetero.1$Distance <- as.numeric(as.character(Hetero.1$Distance))
max(Hetero.1$Distance) # 235.96
length(which(Hetero.1$Distance>=100)) # 93
93/5000

Hetero.5 <- as.data.frame(df.list[[3]])
Hetero.5$Distance <- as.numeric(as.character(Hetero.5$Distance))
max(Hetero.5$Distance) # 237.26
length(which(Hetero.5$Distance>=100)) # 502
502/5000

Hetero.10 <- as.data.frame(df.list[[4]])
Hetero.10$Distance <- as.numeric(as.character(Hetero.10$Distance))
max(Hetero.10$Distance) # 237.75
length(which(Hetero.10$Distance>=100)) # 943
943/5000

Hetero.20 <- as.data.frame(df.list[[5]])
Hetero.20$Distance <- as.numeric(as.character(Hetero.20$Distance))
max(Hetero.20$Distance) # 237.59
length(which(Hetero.20$Distance>=100)) # 1601
1601/5000

Hetero.30 <- as.data.frame(df.list[[6]])
Hetero.30$Distance <- as.numeric(as.character(Hetero.30$Distance))
max(Hetero.30$Distance) # 240.81
length(which(Hetero.30$Distance>=100)) # 2338
2338/5000

