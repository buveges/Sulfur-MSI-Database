# R code utilised in Uveges et al., 2022 - Reconciling discrepant minor sulphur isotope records of the Great Oxidation Event
## DOI: 

## To make review of this code easier, we have arranged the analyses according to the figures that they
##    are associated with in the main text/supplement. For example, the Bayesian statistical analysis
##    code is located under the Fig. S9 heading below

### NOTE: click the arrows to the left of a line of code to expand the collapsed code blocks.
### NOTE: most of the figures that are generated below were further edited using a vector 
###   image editing software (Adobe Illustrator), and as such will appear slightly different
###   to the final images presented in the manuscript.
### NOTE: There is no random "seed" set in this code, meaning that for some analyses that employ 
###     randomized sampling or randomized methods, the outputs from run to run will vary 
###     slightly, but not enough to change any conclusions drawn from the analyses.

## Packages used in this code that need to be pre-loaded
{
library(readxl) # For reading in excel files
library(tidyverse) # General R package family
library(patchwork) # For plotting combination figures
library(grid) # For plotting combination figures
library(MetBrewer) # Color palette package
library(ggridges) # Specialized figure generation package
library(scales)
}
# Setup - loading in and conditioning the database ####

### Coercing incoming data columns to appropriate classes
coltypes <- {c("text","text","text","text","text","text","text","numeric","numeric","numeric",
              "text","text","text","text","text","text","text","numeric","numeric","numeric",
              "numeric","numeric","numeric","numeric","text","text","text","text","numeric")
}
### Reading in database (note you will need to have the .xlsx file in your working directory)
df <- read_excel("New_MIF_database.xlsx", col_types = coltypes)

### Setting phase factor levels and adding a simplified analysis type column, spot/bulk
{
df <- df %>% mutate(Phase = factor(Phase, 
                        levels = c("Sulfide", "Sulfate","Total S","Sulfide + Organic S", "Organic S")),
         Spot.Bulk = if_else(Analysis.Type %in% c("SIMS", "KrF Spot Fluorination, SF6","In-situ Laser","SHRIMP-SI",
                                                  "CO2 Spot Laser FLuorination, SF6","LA-MC-ICP-MS (spot)"),"Spot","Bulk"))
}
### Adding calculated columns of d33 and d36
{
df <- df %>% mutate(d33s = (D33S + 1000*(((1+(d34s/1000))^0.515)-1)),
                    d36s = (D36S + 1000*(((1+(d34s/1000))^1.90)-1)))
}
### Adding binned time column. Bin size = cutW in millions of years.
{
cutW <- 50  # Time bin size
df <- df %>%  mutate(group =cut_width(age.mean,cutW,closed="left"))
df <- df %>% mutate(group.age  = parse_number(as.character(strsplit(as.character(df$group), ",")))+(cutW/2))
}

### Finding data that conform to specific simplified lithologies
{
carb <- grep("limestone|carbonate|dolomite|dolostone|wacke|marble",df$Lithology, ignore.case = TRUE)
hsil <- grep("sand|quartzite|coarse|conglomerate|arenite|aenite|quartz",df$Lithology, ignore.case = TRUE)
lsil <- grep("shale|silt|silstone|mud|phyllit|argillite|schist|marl|pelite",df$Lithology, ignore.case = TRUE)
sulf <- grep("sulfate|evapo|barite|gypsum|anhydrite",df$Lithology, ignore.case = TRUE)
chem <- grep("chert|iron|jasper|BIF|chemical|fe-oxyde|Sed BC|massive sulfide|macro|microscopic sulfide",df$Lithology, ignore.case = TRUE)
microb <- grep("stromat|microb",df$Lithology, ignore.case = TRUE)
alt <- grep("vein|ore body|hydrothermal|colloform|alteration|matatonalite",df$Lithology, ignore.case = TRUE)
glac<- grep("glacial|diamictite|tillite|dropstone|gravity",df$Lithology, ignore.case = TRUE)
ign <- grep("gabbro|basalt|intrusion|volcan|dyke|gneiss|ash|spherule|komati|mafic|dolerite|tuff|hornblende|pyroxene|lava|dacite|amphybollite",df$Lithology, ignore.case = TRUE)
igsed <- grep("ash|tuff|volcanoclastic|silicified volcanic|volcanic sandstone|felsic volcanics",df$Lithology, ignore.case = TRUE)
detr <- grep("detrital",df$Mineralogy, ignore.case = TRUE)
bif <- grep("iron|jasper|BIF",df$Lithology, ignore.case = TRUE)
df <- df %>% mutate(simp.lith = NA)
}
### Assigning simplified lithologies based on above, in new column, Simp.Lith
{
df$simp.lith[carb] <- "Carbonates"
df$simp.lith[hsil] <- "High Energy Siliciclastic"
df$simp.lith[sulf] <- "Sulfates"
df$simp.lith[chem] <- "Chemical"
df$simp.lith[lsil] <- "Low Energy Siliciclastic"
df$simp.lith[microb] <- "Microbial"
df$simp.lith[alt] <- "Altered/Late Addition"
df$simp.lith[glac] <- "Glacial/Diamictite/Detrital"
df$simp.lith[ign] <- "Igneous"
df$simp.lith[igsed] <- "Volcanic Sed"
df$simp.lith[is.na(df$simp.lith)] <- "None Provided"
df$simp.lith[detr] <- "Glacial/Diamictite/Detrital"
}
### Ordering simplified lithology factor levels
{
df <- df %>% mutate(simp.lith = factor(simp.lith, levels = c("High Energy Siliciclastic",
                                                             "Low Energy Siliciclastic","Carbonates",
                                                             "Sulfates","Chemical","Microbial",
                                                             "Altered/Late Addition",
                                                             "Glacial/Diamictite/Detrital","Igneous","Volcanic Sed","None Provided")))
}
### Adding 'Era' column, and setting factor level order
{
df <- df %>% mutate(Era = if_else(age.mean>= 3600, 'Eoarchean',
                                  if_else(age.mean>= 3200, 'Paleoarchean',
                                          if_else(age.mean>= 2800, 'Mesoarchean',
                                                  if_else(age.mean>= 2500, 'Neoarchean',
                                                          if_else(age.mean>= 1600, 'Paleoproterozoic', 'Mesoproterozoic-Present')))))) %>% 
  mutate(Era = factor(Era, levels = c('Eoarchean','Paleoarchean','Mesoarchean','Neoarchean','Paleoproterozoic', 'Mesoproterozoic-Present')))
}

### Creating a "short" data frame. Isolating specific variables and dropping data that don't have D33 data.
#### This is done to make creating the Spot sample averaged database run smoother.
{
dfs33 <- df %>% select(c(age.mean,group,group.age,Sample.ID,
                         d34s,d33s,d36s,D33S,D36S,
                         Phase,Spot.Bulk,Source,
                         Craton,Supergroup,Formation,Core,
                         Date,simp.lith,Era)) %>% 
                drop_na(D33S)
}

### Averaging sample replicates and all spot sample measurements belonging to the same sample (SSA database)
{
dfSSA <- dfs33 %>% group_by(Source,Sample.ID,Spot.Bulk) %>% 
  summarise(age.mean = mean(age.mean),group = unique(group), 
            group.age = unique(group.age), 
            d34s = mean(d34s),d33s = mean(d33s), d36s = mean(d36s,na.rm=TRUE),
            D33S = mean(D33S),D36S = mean(D36S,na.rm=TRUE),an=unique(Spot.Bulk),
            Phase = paste(unique(Phase),collapse = ", "),
            Craton = unique(Craton),Supergroup = unique(Supergroup), 
            Formation = paste(unique(Formation),collapse = ", "),
            Core = unique(Core),
            Date = unique(Date),
            simp.lith = paste(unique(sort(simp.lith)),collapse = ", "),
            Era = unique(Era)) %>% ungroup()
  }

### Fixing some simp.lith issues that result from the same sample used in multiple studies 
{
dfSSA$simp.lith[which(dfSSA$simp.lith == "High Energy Siliciclastic, Glacial/Diamictite/Detrital")] <-"Glacial/Diamictite/Detrital"
dfSSA <- dfSSA%>% mutate(simp.lith = factor(simp.lith, levels = c("High Energy Siliciclastic",
                                                                  "Low Energy Siliciclastic","Carbonates",
                                                                  "Sulfates","Chemical","Microbial",
                                                                  "Altered/Late Addition",
                                                                  "Glacial/Diamictite/Detrital","Igneous/Volcanic Sed","None Provided")))
}

### Setting up the aesthetic themes and color palettes used in the figures
{
theme_ben <- theme_bw()+
  theme(panel.background =  element_rect(color = 'black', size = 1),
        panel.grid = element_blank(),
        text = element_text(size = 7),
        legend.background = element_blank(),
        plot.background = element_rect(fill = "transparent", color="transparent"))
theme_ben2 <- theme_bw()+
  theme(panel.background =  element_rect(color = 'black', size = 1),
        text = element_text(size = 7),
        legend.background = element_blank(),
        plot.background = element_rect(fill = "transparent", color="transparent"))
pal <- MetBrewer::met.brewer(name = "Hokusai1")
}


# Figure 1: Full record and select columns ####
## Creating main ∆33S through time panel
{
big <- ggplot(df)+
    geom_hline(yintercept = 0)+
    geom_rect(aes(ymin=-5,ymax=15,xmin = 2200,xmax=2500), fill = "grey80")+
    geom_point(aes(x=age.mean,y=D33S, 
                   #color = Phase, 
                   shape = Spot.Bulk), color = pal[7])+
    labs(y=expression(Delta^{33}*"S"* " (‰)"), title = "B", x="Age (Myr)",
         shape = "Analysis Type")+
    scale_x_reverse()+
    scale_y_continuous(limits = c(-5,15), expand = c(0,0))+
    scale_color_manual(values = c('black','grey', 'yellow','orange','yellowgreen'))+
    theme_bw()+
    theme(legend.position = c(0.8,0.8),
          plot.title = element_text(margin = margin(t = 0, b = -20),hjust=0.01),
          panel.background =  element_rect(color = 'black', size = 1),
          panel.grid = element_blank(),
          text = element_text(size = 10),
          legend.background = element_blank(),
          plot.background = element_rect(fill = "transparent", color="transparent"))
  #scale_color_manual(values = pal)
}

## Comparing key studies with disparate data (Poulton, Izon, Philippot)

### Pulling out Philippot 2018 data from df
{
phil <- df %>% filter(Source == "Philippot et al., 2018") %>% 
  mutate(Core = factor(Core, levels = c("T3","T2","T1")))
}

### Adding scalar size column
pscale <- phil %>% group_by(Core) %>% summarise(size = max(Depth)-min(Depth)) %>% 
  mutate(scalar = size/(max(size)))

### Tibble for geochronoligcal age data
agetext <- {tibble(x = c(5,5,5),y=c(70,250,160),
                   text = c("> 2,200 Myr","2,312.7 ± 5.6 Myr","< 2,450 Myr"), 
                   Core = c('T3','T2','T1')) %>% 
    mutate(Core = factor(Core, c('T3','T2','T1')))
}

### Tibble for oxygen shading rectangles
oxshadeP <-  {tibble(xmin = c(-Inf,NA,NA),
                     xmax = c(Inf,NA,NA),
                     ymin = c(Inf,NA,NA),
                     ymax = c(140,NA,NA),
                     Core = c('T1','T2','T3')) %>% 
    mutate(Core = factor(Core, c('T3','T2','T1')))
}

### Creating plot faceted by core
{
P <- ggplot(phil)+
    geom_rect(data=oxshadeP,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),fill =  pal[6],alpha=0.3)+
    geom_rect(aes(xmin=-0.3, xmax=0.3, ymin=-Inf, ymax=Inf),alpha = 0.1,fill = 'grey90')+
    #geom_point(aes(y=Depth,x=D33S, color=Source,shape = Analysis.Type),size=2)+
    geom_point(aes(y=Depth,x=D33S, color=Source),size=2, color = pal[3])+
    geom_text(data = agetext, aes(x=x,y=y,label=text), size = 3,color=pal[1])+
    scale_y_reverse(n.breaks = 3)+
    #scale_color_manual(values = "violet")+
    lims(x=c(-1,8))+
    theme_ben+
    labs(y="Depth (m)", x = expression(Delta^{33}*"S"*" (\u2030)"))+
    theme(legend.position = c(0.7,0.35),
          strip.background = element_blank())+
    facet_wrap(~Core,ncol=1, scales = 'free_y', strip.position = "right")
}
### Using grid package so that each facet is scaled to size of core/each other (see pscale output)
{
gt <-  ggplot_gtable(ggplot_build(P))
gt$heights[7] = 0.4*gt$heights[11]
gt$heights[11] = 1*gt$heights[11]
gt$heights[15] = 0.27*gt$heights[11]
}

### Pulling out all data from EBA and KEA cores from data base
{
df2 <- df %>% select(Sample.ID,Depth,Formation,d34s,D33S,D36S,Source,Core,Analysis.Type) %>% 
  filter(Core %in% c("EBA1","EBA2","EBA-1","EBA-2","KEA4","KEA-4")) 

df2 <- df2 %>% mutate(Formation = if_else(Core == "EBA1"&Depth <= 1165,"Lower Timeball Hill",Formation)) %>% 
  mutate(Formation=if_else(Core == "EBA1"& Depth <= 890,"Upper Timeball Hill",Formation))

df2 <- df2 %>% mutate(Formation = if_else(Core == "EBA2"&Depth <= 1335,"Lower Timeball Hill",Formation)) %>% 
  mutate(Formation=if_else(Core == "EBA2"& Depth <= 1100,"Upper Timeball Hill",Formation)) 

dfall <-  df2 %>% mutate(Source = factor(Source, levels = c("Poulton et al., 2021",  "Luo et al., 2016", "Bekker et al., 2004","Izon and Luo et al., 2022")),
           Depth.Scaled = if_else(Core == "EBA1",(Depth - 1165)*-1, 
                                  if_else(Core == "EBA2", (Depth - 1335)*-1 , 
                                          (Depth - 552)*-1)))
}
### Removing EBA1 for clarity
dfall2 <- dfall %>% filter(Core!="EBA1")

### Tibble for ages
agetext2 <- {data.frame(x = c(4.5,5,5,5,4),y=c(-40,10,50,475,10),
                        text = c("< 2,353 ± 18 Myr","2,316 ± 7 Myr","2,310 ± 9 Myr" ,"2,256 ± 6 Myr",
                                 "Rooi.-TBH boundary"), 
                        Core = c('EBA2','EBA2','EBA2','EBA2','KEA4')) 
}
# ##Tibble for oxygen shading rectangles
oxshade <- {tibble(xmin = c(-Inf,-Inf,-Inf,-Inf,-Inf,-Inf,-Inf),
                   xmax = c(Inf,Inf,Inf,Inf,Inf,Inf,Inf),
                   ymin = c(-Inf,19,240,320,390,525,-Inf),
                   ymax = c(5,132,297,325,435,540,5),
                   Core = c('EBA2','EBA2','EBA2','EBA2','EBA2','EBA2','KEA4'))
}
palK <- MetBrewer:: met.brewer('Hokusai1',4)
### Kaapvaal Plot facetted by core
Kap <- {ggplot(dfall2)+
    geom_rect(data=oxshade,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),fill = pal[6],alpha=0.3)+
    geom_hline(yintercept=0)+
    geom_rect(aes(xmin=-0.3, xmax=0.3, ymin=-Inf, ymax=Inf),alpha = 0.1,fill = 'grey90')+
    #geom_point(aes(y=Depth.Scaled,x=D33S, color=Source,shape=Analysis.Type),size=2)+
    geom_point(aes(y=Depth.Scaled,x=D33S, color=Source),size=2)+
    geom_text(data = agetext2, aes(x=x,y=y,label=text), size = 3,color=pal[1])+
    scale_color_manual(values = palK)+
    lims(x=c(-1,8))+
    #geom_vline(xintercept=0.3,linetype='dashed')+
    facet_wrap(~Core)+
    theme_ben+
    labs(y="Depth Scaled to Roo.-TBH Boundary (m)", x = expression(Delta^{33}*"S"*" (\u2030)"))+
    theme(legend.position = c(0.82,0.7),
          strip.background = element_blank())
}

### Adding just summary ∆33S plot to top
    # Setting layout for patchwork
{
design <- " 
  11111
  22233
  22233
"
big+Kap+gt+plot_layout(design=design)
}
### NOTE: For this version of the figure you will need the magick and ggpubr packages
#### Importing Oxygen History PNG
OxH <- magick::image_read("/Users/benjaminuveges/Dropbox/GOE_sulfur_data/SMIF Compilation/New Database/Oxygen_history.png")
#### Creating ggplot object from PNG
OxH <- {ggplot() +
  ggpubr::background_image(OxH)+
  theme_ben
}

### Adding Oxygen history PNG to the top of plot
{
design <- "
  11111
  22222
  33344
  33344
"
OxH+ big+Kap+gt+plot_layout(design=design)
}





# Figure 2: Cumulative mean, and evolving proportions plot ####

## Calculating the cumulative mean and number of samples for the full database binned by publication year
{
dateM <- dfs33 %>% filter(age.mean > 2300) %>% drop_na(D33S) %>% group_by(Date) %>% 
  summarise(n=n(),m=mean(D33S),s=sum(D33S)) %>% mutate(cn=cumsum(n) ,cs=cumsum(s), Crat.Test = "All") %>% 
  mutate(M=cs/cn, Data = "All Data") 
}

## Same for the bulk/sample averaged (SSA) database
{
dateMB <- dfSSA %>% filter(age.mean > 2300) %>% drop_na(D33S) %>% group_by(Date) %>% 
  summarise(n=n(),m=mean(D33S),s=sum(D33S)) %>% mutate(cn=cumsum(n) ,cs=cumsum(s), Crat.Test = "All") %>% 
  mutate(M=cs/cn, Data = "Bulk/Sample Ave") 
}

## Calculating the cumulative mean and number of samples Kaapvaal+Pilbara vsall other cratons, Full data
### Labelling data
{
dfs <- dfs33 %>% mutate(Crat.Test = if_else(Craton %in% c("Kaapvaal","Pilbara"),
                                            "Kaapvaal + Pilbara","Other"))

dateMC <- dfs %>% filter(age.mean > 2300,Crat.Test != "Other") %>% drop_na(D33S) %>% group_by(Date) %>% 
  summarise(n=n(),m=mean(D33S),s=sum(D33S),Crat.Test=unique(Crat.Test)) %>% mutate(cn=cumsum(n) ,cs=cumsum(s)) %>% 
  mutate(M=cs/cn, Data = "All Data") 


dateMC2 <- dfs %>% filter(age.mean > 2300,Crat.Test == "Other") %>% drop_na(D33S) %>% group_by(Date) %>% 
  summarise(n=n(),m=mean(D33S),s=sum(D33S),Crat.Test=unique(Crat.Test)) %>% mutate(cn=cumsum(n) ,cs=cumsum(s)) %>%
  mutate(M=cs/cn, Data = "All Data") 
}

## Same process but for SSA database
{
dfsb <- dfSSA %>% mutate(Crat.Test = if_else(Craton %in% c("Kaapvaal","Pilbara"),
                                             "Kaapvaal + Pilbara","Other"))

dateMCb <- dfsb %>% filter(age.mean > 2300,Crat.Test != "Other") %>% drop_na(D33S) %>% group_by(Date) %>% 
  summarise(n=n(),m=mean(D33S),s=sum(D33S),Crat.Test=unique(Crat.Test)) %>% mutate(cn=cumsum(n) ,cs=cumsum(s)) %>% 
  mutate(M=cs/cn, Data = "Bulk/Sample Ave") 

dateMC2b <- dfsb %>% filter(age.mean > 2300,Crat.Test == "Other") %>% drop_na(D33S) %>% group_by(Date) %>% 
  summarise(n=n(),m=mean(D33S),s=sum(D33S),Crat.Test=unique(Crat.Test)) %>% mutate(cn=cumsum(n) ,cs=cumsum(s)) %>% 
  mutate(M=cs/cn, Data = "Bulk/Sample Ave") 
}
## Binding calculated cummulative datasets
dateALL <- rbind(dateM,dateMB,dateMC,dateMC2,dateMCb,dateMC2b)

## Pivoting dateALL for easier calculation of percentages later
dateSplit <- dateALL %>% select(Date,cn,Crat.Test,Data) %>% pivot_wider(names_from = Crat.Test,values_from = c(cn)) 

## Quick for loop to plug in holes where "Other" doesn't have data.
for (i in 1:length(dateSplit$`Other`)) {
  
  if(is.na(dateSplit$`Other`[i]))
    dateSplit$`Other`[i] = dateSplit$`Other`[i-1]
}
## Adding Percent column
dateSplit <- dateSplit %>% mutate(KPper = ((`Kaapvaal + Pilbara`)/`All`)*100, Oper = ((`Other`)/`All`)*100)

## Isolating data for bar chart
dateBar <- dateSplit %>% select(Date,Data, KPper,Oper) %>% gather(key=KPO,value = per, -c(Date,Data)) %>% 
  filter(Data == "All Data")


## Generating synthetic data sets
### Temporally adjusted synthetic dataset (TA)
{
lengths <- dfSSA %>% group_by(group) %>% summarise(length(D33S))
N <- max(lengths[[2]])

samples <- dfSSA %>% 
  group_by(group) %>% 
  sample_n(size = N, replace=TRUE) %>% ungroup()

samples2 <- samples[which(samples$group.age>=2300),] %>% 
  mutate(weight = exp(1)^(-0.001*(group.age-2300-1)))
}
### Craton Adjusted synthetic dataset (CA)
{
samplesb <- dfSSA %>% drop_na(Craton) %>% 
  filter(simp.lith !="Altered/Late Addition") %>% 
  group_by(Craton) %>% 
  sample_n(size = N, replace=TRUE) %>% ungroup()

samplesb <- samplesb %>%
  group_by(group) %>% 
  sample_n(size = N, replace=TRUE) %>% ungroup()

samplesb2 <- samplesb[which(samplesb$group.age>=2300),] %>% 
  mutate(weight = exp(1)^(-0.001*(group.age-2300-1)))
}

### Quick tibble to summarise the means of the weighted synthetic datasets.
Synth <- {tibble(Dataset = c("TA", "CA"), mean = c(weighted.mean(samples2$D33S,samples2$weight),
                                                  weighted.mean(samplesb2$D33S,samplesb2$weight)),
                Date=c(2022,2022))
}

# Stacked bar chart comparing percentage of Kaapvaal+Pilbara vs all other cratons
{
b <- ggplot(dateBar,aes(fill = KPO,x=Date, y=per))+
  geom_bar(position='stack',stat="identity")+
  geom_hline(yintercept = 50, linetype='dashed', color='grey65', size=1)+
  labs(x=element_blank(), y = "Cummulative Percentage (%)",fill = "Craton",title="B")+
  scale_y_continuous(expand = c(0,0))+
  #scale_x_discrete(position = "top")+
  scale_x_continuous(position = "top")+
  #scale_fill_manual(values = c("#A31F34",'skyblue',"#8A8B8C"))+
  scale_fill_manual(values = c(pal[1],pal[6]))+
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(margin = margin(t = 0, 
                                                  b = -40),hjust=0.01),
        axis.text.x = element_blank(),
        panel.background =  element_rect(color = 'black', size = 1),
        panel.grid = element_blank(),
        text = element_text(size = 5),
        legend.background = element_blank(),
        plot.background = element_rect(fill = "transparent", color="transparent"))
}
# Cumulative mean plot. Full vs SSA for all the data, and the Kaapvaal+Pilbara vs all other cratons
{
t <- ggplot(dateALL)+
  geom_vline(xintercept = 2012, color = 'grey', size=5,alpha=0.7)+
  geom_hline(yintercept = 0.3, linetype='dashed')+
  geom_path(aes(x=Date,y=M, color=Crat.Test,linetype=Data),size=1)+
  geom_point(data=Synth, aes(x=Date,y=mean, shape = Dataset),size=2, color=pal[7])+
  geom_text(data=Synth, aes(x=Date,y=mean,label = Dataset), size=1.5,nudge_x = -1,color=pal[7])+
  theme_bw()+
  theme(legend.position = c(0.53,0.13),
        legend.box = 'horizontal',
        plot.title = element_text(margin = margin(t = 0, b = -20),hjust=0.01),
        #axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        panel.background =  element_rect(color = 'black', size = 1),
        panel.grid = element_blank(),
        text = element_text(size = 5),
        legend.background = element_blank(),
        plot.background = element_rect(fill = "transparent", color="transparent"))+
  labs(color="Craton", y = expression("Cumulative Mean "* Delta^{33}*"S"* " (‰)"),linetype = 'Database', title = "A")+
  scale_color_manual(values = c(pal[2],pal[1],pal[6]))+
  guides(shape = "none")
}

# 'Era' distribution full database,
EraL <-{ df %>% drop_na(D33S) %>% group_by(Era) %>% summarise(count=n()) %>% 
  ungroup() %>% mutate(per = (count/sum(count))*100) 
}
{
r <- ggplot(df)+
  geom_bar(aes(y=Era, fill=Era))+
  scale_fill_manual(values =  met.brewer('Hokusai1',6))+
  #scale_color_manual(values = c(NA,NA,NA,NA,NA,"black"))+
  labs(title="C", x = "Number of Analyses")+
  geom_label(data=EraL, aes(y=Era, x = 2000, label=Era),size = 2)+
  scale_x_continuous(expand = expansion(add=c(10,100)))+
  theme_bw()+
  theme(legend.position = "none", 
        #axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(margin = margin(t = 0, b = -20),hjust=0.98),
        panel.background =  element_rect(color = 'black', size = 1),
        panel.grid = element_blank(),
        text = element_text(size = 5),
        legend.background = element_blank(),
        plot.background = element_rect(fill = "transparent", 
                                       color="transparent"))
}

## Generating combination plot
{
design <- "
  113
  113
  223
"
t+b+r+plot_layout(design = design)
}


# Figure 3: CME model output  ####

## Importing matlab output files
{
### Time Adjusted Synthetic database bootstrap output for Weathered S
regboot <- read_excel("Synthetic Datasets/boot_out5.xlsx")%>% mutate(output = 'boot')%>% 
  select(age,D33Scalc,D33Sfit,D33Sfitup,D33Sfitlow,output)
### Seawater sulfate reservoir (SWSR) composition
regpool <- read_excel("Synthetic Datasets/pool5.xlsx")%>% mutate(D33Scalc = D33S,output = 'pool') %>% 
  select(age,D33Scalc,D33Sfit,D33Sfitup,D33Sfitlow,output)
### Joining weathered and seawater sulphur pools into one dataframe
RegPool <- inner_join(regboot,regpool)
}
## Same as above jbut for the Time and Craton Adjusted synthetic database
{
cratboot <- read_excel("Synthetic Datasets/boot_outCraton2.xlsx")%>% mutate(output = 'boot') %>% mutate(output = 'boot')%>% 
  select(age,D33Scalc,D33Sfit,D33Sfitup,D33Sfitlow,output)

cratpool <- read_excel("Synthetic Datasets/pool_Craton2.xlsx")%>% mutate(D33Scalc = D33S,output = 'pool') %>% 
  select(age,D33Scalc,D33Sfit,D33Sfitup,D33Sfitlow,output)

CratPool <- inner_join(cratpool,cratboot)
}
## Same as above but for the unadjusted SSA database
{
uwboot <- read_excel("Synthetic Datasets/boot_out_noweightSSA.xlsx")%>% mutate(output = 'boot')%>% 
  select(age,D33Scalc,D33Sfit,D33Sfitup,D33Sfitlow,output)
uwpool <- read_excel("Synthetic Datasets/pool_noweightSSA.xlsx")%>% mutate(D33Scalc = D33S,output = 'pool') %>% 
  select(age,D33Scalc,D33Sfit,D33Sfitup,D33Sfitlow,output)
}

## Plot of TA Synthetic dataset outputs
{
### Label tibble
reglabs <- tibble(x = c(2.9,2.3,2.0), y= c(1.4,1.0,0.15), 
                  text = c("Reinhard et al. CME","Weathered Sulfur", "SWSR"))
### Plot
reg <- ggplot(regboot,aes(x=age))+
  geom_hline(yintercept = c(-0.3,0.3),linetype='dashed',alpha = 0.5)+
  geom_hline(yintercept=1.5, size=3, color = "grey")+
  geom_ribbon(aes(ymin = D33Sfitlow,ymax=D33Sfitup),fill = pal[7],alpha = 0.2)+
  #geom_ribbon(data = regpool %>% drop_na(),aes(x=age, ymin=D33Sfitlow,ymax=D33Sfitup),fill=pal[2],alpha = 0.2 )+
  geom_ribbon(data = regpool,aes(x=age,ymax=D33Sfitup,ymin=D33Sfitlow),fill=pal[2],alpha=0.2)+
  #geom_point(aes(y=D33Scalc),color=pal[7],alpha = 0.1)+
  geom_path(aes(y=D33Sfit),color=pal[7],size=1)+
  theme_bw()+
  geom_path(data=regpool,aes(x=age,y=D33Sfit),col=pal[2],size=1)+
  geom_text(data=reglabs,aes(x=x,y=y,label=text), color = c("grey",pal[7],pal[2]))+
  #ylim(-1.5,1.5)+
  ylim(-1.6,3)+
  xlim(c(3.1999,1.78))+
  labs(title = "A - TA Dataset",x="Age (Ga)",y = expression(Delta^{33}*"S"* " (‰)"))+
  theme_ben+
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank())
}
## Plot of CA synthetic dataset outputs
{
### Label tibble
cratlabs <- tibble(x = c(2.9,2.3,2.0), y= c(1.4,0.65,0.0), 
                   text = c("Reinhard et al. CME","Weathered Sulfur", "SWSR"))
### Plot
crat <- ggplot(cratboot,aes(x=age))+
  geom_hline(yintercept = c(-0.3,0.3),linetype='dashed',alpha = 0.5)+
  geom_hline(yintercept=1.5, size=3, color = "grey")+
  geom_ribbon(aes(ymin = D33Sfitlow,ymax=D33Sfitup),fill=pal[7],alpha = 0.2)+
  geom_ribbon(data = cratpool,aes(x=age, ymin=D33Sfitlow,ymax=D33Sfitup),fill=pal[2],alpha = 0.2 )+
  #geom_point(aes(y=D33Scalc),color=pal[7],alpha = 0.025)+
  geom_path(aes(y=D33Sfit),color=pal[7],size=1)+
  theme_bw()+
  geom_path(data=cratpool,aes(x=age,y=D33Sfit),col=pal[2],size=1)+
  geom_text(data=cratlabs,aes(x=x,y=y,label=text), color = c("grey",pal[7],pal[2]))+
  #ylim(-1.5,1.5)+
  ylim(-1.6,3)+
  xlim(c(3.1999,1.78))+
  labs(title = "B - CA Dataset",x="Age (Ga)",y = expression(Delta^{33}*"S"* " (‰)"))+
  theme_ben+
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank())
}
## Plot of unadjusted SSA database output

{
### Label tibble
uwlabs <- tibble(x = c(2.9,2.3,2.0), y= c(1.4,2.1,0.0), 
                 text = c("Reinhard et al. CME","Weathered Sulfur", "SWSR"))
### Plot
uwMod <- ggplot(uwboot,aes(x=age))+
  geom_hline(yintercept = c(-0.3,0.3),linetype='dashed',alpha = 0.5)+
  geom_hline(yintercept=1.5, size=3, color = "grey")+
  geom_ribbon(aes(ymin = D33Sfitlow,ymax=D33Sfitup),fill=pal[7],alpha = 0.2)+
  geom_ribbon(data = uwpool,aes(x=age, ymin=D33Sfitlow,ymax=D33Sfitup),fill=pal[2],alpha = 0.2 )+
  #geom_point(aes(y=D33Scalc),color=pal[7],alpha = 0.025)+
  geom_path(aes(y=D33Sfit),color=pal[7],size=1)+
  theme_bw()+
  geom_path(data=uwpool,aes(x=age,y=D33Sfit),col=pal[2],size=1)+
  geom_text(data=uwlabs,aes(x=x,y=y,label=text), color = c("grey",pal[7],pal[2]))+
  ylim(-1.6,3)+
  xlim(c(3.1999,1.78))+
  labs(title = "C - Unadjusted SSA Dataset",x="Age (Ga)",y = expression(Delta^{33}*"S"* " (‰)"))+
  theme_ben+
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank())
}
## Full combination plot
reg+crat+uwMod


# Figure 4: Model run for just the Pilbara, compared to Philippot et al. (2018) ####

## Reading in model out put and pulling out data of Philippot et al. (2018)
out <- read_excel("Synthetic Datasets/Pilbara_out.xlsx")%>% mutate(output = 'boot')
phil <- df %>% filter(Source=="Philippot et al., 2018")

## Indiidual panel plots
{
### Main panel showing model output and Pilbara craton data
m <- ggplot(out)+
  geom_ribbon(aes(x=age*1000, ymax = D33Sfitup,ymin = D33Sfitlow ), fill = pal[7], alpha = 0.5)+
  geom_path(aes(x=age*1000, y=D33Sfit),color=pal[7])+
  geom_vline(xintercept = 2.33*1000, linetype='dashed')+
  geom_hline(yintercept = 0.25,linetype='dashed')+
  geom_point(data = df %>% filter(Craton == "Pilbara", Source!="Philippot et al., 2018",age.mean >1800 & age.mean<3200), 
             aes(x = age.mean,y=D33S),color = pal[7])+
  geom_point(data=df %>% filter(Source == "Philippot et al., 2018"),aes(x=age.mean, y = D33S), color = pal[2], alpha = 0.2)+
  annotate(geom= "rect",xmin = 2000,xmax=2500, ymin=-0.75, ymax=2.5, fill=NA, color="black")+
  scale_x_reverse()+
  labs(x= "Age (Ma)", y = expression(Delta^{33}*"S"*" (\u2030)"),title = "A")+
  theme_ben+
  theme(plot.title = element_text(margin = margin(t = 0, 
                                                  b = -20),hjust=0.01))
### Insert zoom in on 2.5-2.0 Ga
i <- ggplot(out)+
  geom_vline(xintercept = 2.33*1000, linetype='dashed')+
  geom_hline(yintercept = 0.25,linetype='dashed')+
  geom_ribbon(aes(x=age*1000, ymax = D33Sfitup,ymin = D33Sfitlow ), fill = pal[7], alpha = 0.5)+
  geom_path(aes(x=age*1000, y=D33Sfit))+
  geom_point(data=df %>% filter(Source == "Philippot et al., 2018"),aes(x=age.mean, y = D33S), color = pal[2], alpha = 0.2)+
  lims(x=c(2500,2000))+
  labs(x= "Age (Ma)", y = expression(Delta^{33}*"S"*" (\u2030)"))+
  theme_ben
### Phillipot data density plots
Ts <-ggplot(phil)+
  annotate(geom='rect',xmin=-Inf,xmax=Inf, ymax = 0.68+0.5,ymin=0.68-0.5,fill = pal[7],alpha = 0.5)+
  geom_hline(yintercept = 0.7, color=pal[7])+
  geom_density(aes(y=D33S), bw = 0.075, fill = pal[2], color = NA)+
  scale_x_continuous(expand = expansion(add=c(0,0.1)))+
  facet_wrap(~Core, scales = 'free_x')+
  labs(x= element_blank(), y = expression(Delta^{33}*"S"*" (\u2030)"),title = "B")+
  theme_ben+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.background = element_blank())
}
## Combining into full figure
{
design <- "
  1
  1
  2
"
(m+inset_element(i,0.55,0.52,1,1,align_to = "full"))+Ts+plot_layout(design=design)
}







# Figure 5: Specifc EBA2 intervals and bootstrap test. ####

## Bootstrap analysis of synthetic cores based on the EBA2 data of Poulton et al. (2021)

### Extracting and parameterizing Poulton et al. (2021) data
{
dfB <- df %>% filter(Source =="Poulton et al., 2021")
#### Removing EBA-1, SF6 duplicate measurements, and anything above Gatsrand, then characterizing S-MIF statusof each sample
cutoff <- 0.3  # Cutoff for S-MIF
LTB_MIF <- dfB %>% filter(Depth > 1150, Core == "EBA2",Formation == 'Timeball Hill') %>% 
  mutate(tf=if_else(D33S<=cutoff,FALSE,TRUE))
MIFd <- LTB_MIF %>% select(Core,Depth,D33S) %>% 
  mutate(tf=if_else(D33S<=cutoff,FALSE,TRUE)) %>% 
  filter(tf==TRUE) %>% mutate(Depth = Depth - min(LTB_MIF$Depth),xline=2)

int_range <- max(LTB_MIF$Depth)-min(LTB_MIF$Depth) # Range of interval of interest (to simplify depth)

stepD <- 0.05 # Step width (typical sample size taken from cores), 0.05 = 5cm 
}

### Creating core that is 'int_range' long with 'samples' every stepD, In this case, 150 m and every 5 cm
{
depth <-  seq(stepD,int_range,stepD) 
fd <- data.frame(depth = depth) # Converting to data frame
}
### Setting sampling parameters
{
N <- 60 # Number of random "samples" to pull from fake core
reps <- 1000 # Number of 'batches' of samples to pull (ie N new samples each time)
Wind <- data.frame(w=c(0.02,0.2,0.5,1,1.42,2,5,10)) # Dataframe of Windows to use iteratively
#### Preallocating S-MIF count output table
MIF_count <- tibble(Window = Wind$w, Zero_MIF = NA, MIFWindow = "m MIF layers") %>% 
  unite(MIFWindow,c("Window","MIFWindow"),sep=" ")
fdtest <- fd  %>% mutate(MIF=FALSE) %>% mutate(xline=0)
}

### Creating numerous cores with different windows (in Wind df) in same data frame ###
out <- data.frame() # pre-allocating output dataframe

for(k in 1:length(Wind$w)){
  for(j in 1:length(fdtest$depth))
  {
    for (i in 1:length(MIFd$Depth))
    {
      fdtest$MIF[j] <-  if_else((fdtest$depth[j] <= (MIFd$Depth[i]+(Wind$w[k]/2)) & fdtest$depth[j] >= (MIFd$Depth[i]-(Wind$w[k]/2))), TRUE,
                                if_else(fdtest$MIF[j]==TRUE,TRUE,FALSE))
    }
    out <- rbind(out,c(fdtest$MIF[j],fdtest$depth[j],Wind$w[k]))
  }
}

#### Assigning column names
out <- out %>% mutate(depth = out[[2]],num =  out[[1]],w=out[[3]], xline = 0) %>% mutate(MIF = if_else(num==1,TRUE,FALSE))

fdtest <- fdtest %>% mutate(num = if_else(MIF==TRUE,1,0))

### Randomly sampling synthetic cores 60 times, in 1000 replicate batches, then assinging replicate column
{
g3 <-  replicate(reps,sample_n(group_by(out,w),N,replace = FALSE),simplify=FALSE) %>% 
  bind_rows() %>%
  mutate(replicate = 1:n(),group=NA)

for (n in 1:(length(g3$replicate)/N)) {
  g3$group[((n*N)-(N-1)):(N*n)] <- n
}
}
### Summarizing the number of MIF samples present in each group
{
get_mif<- g3 %>% group_by(w,group) %>% summarise(mif_count=sum(num))

gmmean <- get_mif %>% group_by(w) %>% summarise(m=round(mean(mif_count)),s=round(sd(mif_count)))

gmsum <- get_mif %>% filter(mif_count == 0 ) %>% group_by(w) %>% summarise(Zero = n()) %>% mutate(Percentage = (Zero/reps)*100)

gmsum2 <- data.frame(w=c(2,5,10),Zero = c(0,0,0), Percentage = c(0,0,0))

gmsum <- as.data.frame(rbind(gmsum,gmsum2))

get_mif <- get_mif %>% mutate(Per = (mif_count/60)*100)
}

### Label tibble
ridgetext <- {tibble(x = c(30.5,0), y = c(1.42,0.02),
                    text = c("--Poulton et al. (2021)", 
                             "-- Izon and Luo et al, (2022)"))
}
### Plot
{
rid <- ggplot(get_mif,aes(x=Per, y = factor(w), 
                          fill = factor(w),
                          vline_color = ..quantile..))+
  #geom_vline(xintercept = 30.5, linetype = 'dashed', col = pal[6],size=1.5)+
  geom_density_ridges(scale = 2, rel_min_height = 0.01,
                      quantile_lines=TRUE, bandwidth = 2,color=NA)+
  theme_ben2+
  #scale_y_discrete(limits = rev(levels(dfSSA$simp.lith)))+
  scale_discrete_manual("vline_color",
                        values = c("white", "white", "white", "white"), 
                        name = NULL)+
  scale_x_continuous(limits = c(0,75), expand = c(0,0))+
  #scale_fill_viridis_d(option = "D", direction = 1)+
  scale_fill_manual(values = MetBrewer::met.brewer(name = "Hokusai1",8))+
  theme(legend.position='none')+
  labs(x="Percentage (%) of S-MIF-Bearing Samples",fill = "Simplified Lithology", y = "Average Thickness of S-MIF Window (m)" )+
  ggtitle("A")+
  theme(plot.title = element_text(margin = margin(t = 0, b = -20),hjust=0.01),
        panel.grid.minor = element_blank())

}

## Comparison of EBA2 data from this study and that of Poulton et al. (2021)

### Poulton et al. (2021) data
small <- {dfall %>% filter(#Analysis.Type =="Bulk, EA-CF-IRMS", 
  Core == "EBA2", 
  Source == "Poulton et al., 2021") %>% 
    group_by(Sample.ID) %>% 
    summarise(Depth = mean(Depth), d34s = mean(d34s),D33S = mean(D33S),D36S = mean(D36S,na.rm=TRUE),
              Formation = paste(unique(Formation),collapse = ", "),
              Core = unique(Core), Depth.Scaled = mean(Depth.Scaled), Source = unique(Source)) %>% ungroup()
}
small <- small %>% select(-Depth.Scaled,-Formation,)

### Reading in this studies data and binding it into a new dataframe with that of Poulton et al. (2021)
BTU <- read_excel("Uveges TableSXv2.xlsx", sheet=2) %>% select(Sample.ID,Depth,d34s,D33S,D36S,Core,Source)
small <- rbind(small,BTU)

### Figure
#### Boxes locating blow ups
{
rects <- {tibble(xmin=c(-0.3,-0.3,-0.3,-0.3), 
                 xmax=c(3,3,3,3), 
                 ymin=c(1335,1276 ,1013,810), 
                 ymax=c(1330,1274.5 ,1011,802),
                 range = ymin-ymax)
}
intlabs <-{ tibble(ymin = c(1332,1275.36,1011,806.8,802),
                   ymax = c(1334.5,1275.7,1013,809.2,803.5),
                   x = c(1,1,1.5,1.5,1),
                   labs = c("3 cm and 1.4 m","~40 cm","2 m","2.5 m","1.5 m"),
                   box = c(4,3,2,1,1)) %>% 
    mutate(y = (ymin+ymax)/2)
}

smallZoom <- {small %>% 
    mutate(box = if_else(Depth <= rects$ymin[1] & Depth >= rects$ymax[1],4,
                         if_else(Depth <=rects$ymin[2] & Depth>= rects$ymax[2],3,
                                 if_else(Depth <=rects$ymin[3] & Depth >= rects$ymax[3],2,
                                         if_else(Depth <=rects$ymin[4] & Depth >= rects$ymax[4],1,5))))) %>% 
    filter(box<5)
}
box2 <- {ggplot(smallZoom %>% arrange(Depth))+
    geom_rect(aes(xmin=-0.3, xmax=0.3, ymin=-Inf, ymax=Inf),alpha = 0.1,fill = 'grey90')+
    geom_point(aes(x=D33S,y=Depth, color = Source, shape = Source),size = 2)+
    #geom_path(aes(x=D33S,y=Depth))+
    geom_segment(data = intlabs, aes(x = x,xend=x, y = ymin, yend=ymax), color = pal[2])+
    geom_text(data = intlabs,aes(x=x,y=y,label=labs), color = pal[2], hjust =0, nudge_x = 0.02 )+
    scale_color_manual(values = c(pal[7],pal[2]))+
    scale_y_reverse(n.breaks=4,position = 'right')+
    labs(x = expression(Delta^{33}*"S"*" (\u2030)"))+
    facet_wrap(~box, scales="free_y",ncol=1, strip.position ="right")+
    theme_ben+
    theme(strip.text = element_blank(),
          strip.background = element_blank(),
          legend.position="none",
          plot.title = element_text(margin = margin(t = 0, b = -20),hjust=0.01),
          axis.title.y = element_blank())
}
box2 <-  ggplot_gtable(ggplot_build(box2))
box2$heights[7] = (rects$range[4]/rects$range[4])*box2$heights[7]
box2$heights[11] = (rects$range[3]/rects$range[4])*box2$heights[7]
box2$heights[15] = (rects$range[2]/rects$range[4])*box2$heights[7]
box2$heights[19] = (rects$range[1]/rects$range[4])*box2$heights[7]
}

#### Main data summary plot
{
allP <- ggplot(small)+
  #geom_hline(yintercept=0)+
  geom_rect(aes(xmin=-0.3, xmax=0.3, ymin=-Inf, ymax=Inf),alpha = 0.1,fill = 'grey90')+
  #geom_rect(aes(xmin=-0.3, xmax=3, ymin=0, ymax=170), fill = 'transparent',color='black')+
  geom_point(aes(y=Depth,x=D33S, color=Source, shape = Source),size=2)+
  geom_rect(data=rects,aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill='transparent',col='black')+
  scale_color_manual(values = c(pal[7], pal[2]))+
  #geom_vline(xintercept=0.3,linetype='dashed')+
  scale_y_reverse(n.breaks=6)+
  theme_ben+
  theme(legend.position = c(0.75,0.67), strip.background = element_blank(),
        legend.background = element_blank(),
        #legend.text = element_text(size=5),
        #legend.title = element_text(size=7),
        plot.title = element_text(margin = margin(t = 0, b = -20),hjust=0.01)
  )+
  labs(y="Depth (m)", 
       x = expression(Delta^{33}*"S"*" (\u2030)"),
       title = "B")
}

#### Inset element
{
ten <- out %>% filter(w==10)
small <- small %>% mutate(MIF = if_else(D33S > 0.3, TRUE,FALSE))
seg <- data.frame( ymax = (Wind$w/2)+1224.00+0.025,  ymin = 1224.00-0.025-(Wind$w/2), w=Wind$w, x = seq(0.7,0,length.out=8))

i2 <-  ggplot()+
  #geom_point(data = small %>% filter(MIF==FALSE), aes(x=D33S,y=Depth), color='grey')+
  #geom_point(data = small %>% filter(MIF==TRUE), aes(x=D33S,y=Depth), color = pal[7])+
  geom_point(data = ten %>% filter(MIF==FALSE), aes(y=depth+1335-152, x=0), color='grey')+
  geom_point(data = ten %>% filter(MIF==TRUE), aes(y=depth+1335-152, x=0), color = pal[7])+
  geom_segment(data = seg, aes(x=x, xend =x, y = ymin, yend=ymax,color = factor(w)),size=2)+
  scale_color_manual(values = MetBrewer::met.brewer(name = "Hokusai1",8))+
  ylim(1224+6,1224-6)+
  xlim(-0.2,0.75)+
  theme_ben+
  theme(legend.position = 'none',
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())
  }

#### Full combination plot
design <- "
  11223
"
(rid+ inset_element(i2,0.55,0.05,0.98,0.375,align_to = "plot"))+allP +box2+ plot_layout(design = design)





# Fig S5 Bootstrap Sensitivity Test ####
tt <- 2300 # Time cutoff
## number repeat batches (NOTE that the 500 and 1000 m and n replicate tests have been removed here for brevity)
##    Can uncomment the line below the m and n definition to put them back in
m <- c(5,10,20,50,100)
# m <- c(5,10,20,50,100, 500, 1000)
## number of samples pulled
n <- c(5,10,20,50,100)
# n <-  <- c(5,10,20,50,100, 500, 1000)

## Setting time resampled data base
{
samplesT <-  samples[which(samples$group.age>(tt)),] %>% 
  mutate(weight = exp(1)^(-0.001*(group.age-(tt-1))))
prob <- samplesT$weight
}
## Dummy tibble for for-loop output
weathWeight33out <- tibble(d34s=NA,d33s=NA,D33S=NA,replicate=NA,m=NA,n=NA,run=NA)

## Bootstrap mean function
mean_func33 <- function(x) {
  x %>% 
    summarise(D33S = mean(x[[21]], na.rm=TRUE))
} 

## for-loop set up
for(h in 1:10){
  for(i in 1:length(m)){
    for(j in 1:length(n)){
      
      weathWeight33 <- replicate(m[i], sample_n(samplesT,n[j],weight = prob, replace = TRUE), simplify = FALSE) %>%
        lapply(., mean_func33) %>% 
        bind_rows() %>%
        mutate(replicate = 1:n(), m = m[i], n=n[j],run=h)
      
      weathWeight33out <- bind_rows(weathWeight33out,weathWeight33)
      
    }
  }
}

## Calculating stats from sensitivty test
sensStats <- {weathWeight33out %>% drop_na(n,m,run) %>% group_by(n,m,run) %>% summarise(mean = mean(D33S), sd = sd(D33S)) %>% 
  mutate(total = n*m) }

## Plots
{
pS <- ggplot(sensStats)+
  geom_smooth(aes(x=n,y=sd), color="grey",linetype="dashed")+
  geom_point(aes(x=n,y=sd,color=factor(m)),size=2)+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_color_manual(values = pal)+ 
  #scale_color_viridis_d(option="E")+
  annotation_logticks(sides="b")+
  labs(x= "Number of samples grabbed per batch (n)", y="Standard deviation of resultant means",
       color= "Number of batches (m)",title="B")+
  theme_ben+theme(legend.position = c(0.85,0.65), 
                  legend.background = element_blank(),
                  text=element_text(family="Helvetica"))

pM <- ggplot(sensStats)+
  geom_point(aes(x=n,y=mean,color=factor(m)),size=2)+
  #geom_hline(yintercept = weighted.mean(samplesT$D33S,samplesT$weight,na.rm=TRUE),color='grey',linetype='dashed',size=1.5)+
  geom_hline(yintercept = mean(sensStats$mean),color='grey',linetype='dashed',size=1.5)+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_color_manual(values = pal)+ 
  #scale_color_viridis_d(option="E")+
  annotation_logticks(sides="b")+
  labs(x= element_blank(), y="Mean of resultant means",
       color= "Number of batches (m)",title="A")+
  theme_ben+theme(legend.position = 'none', legend.background = element_blank())
  }

## Combo Plot
pM/pS


# Fig S6 Testing simplified lithology bias sensitivity ####

### Summarising simp.lith groups
{
LithCounts <- df %>% filter(age.mean>2200) %>% group_by(simp.lith) %>% 
  summarise(mean = mean(D33S,na.rm=TRUE),count=n()) %>% ungroup() %>% mutate(Frac = count/sum(count))

LithCountsA <- dfSSA %>% filter(age.mean>2200) %>% group_by(simp.lith) %>% 
  summarise(mean = mean(D33S,na.rm=TRUE),count=n()) %>% ungroup() %>% mutate(Frac = count/sum(count))
}
### Pre-allocating output dataframe
outLith <- {tibble(Lith = NA, mean = NA, randL = NA, randM=NA, 
                   randH=NA, fracL = NA, fracM = NA, fracH = NA, run = NA)}
### Generating randomized weights for lithology weighted mean calculations
for (j in 1:1000) {
  a <- LithCounts %>% select(simp.lith,mean) %>% 
    mutate(randL = runif(11,min=1,max=100)) %>% 
    mutate( randM= if_else(simp.lith=="Low Energy Siliciclastic", randL+300,randL), fracL=randL/sum(randL)) %>%
    mutate( randH= if_else(simp.lith=="Low Energy Siliciclastic", randL+900,randL), fracM=randM/sum(randM)) %>%
    mutate(fracH=randH/sum(randH) ,run = j)
  outLith <- bind_rows(outLith,a)
}
{
Test <- outLith %>% group_by(run) %>% 
  summarise(WM = weighted.mean(mean,fracL),
            LES = fracL[which(simp.lith=="Low Energy Siliciclastic")],
            WMm = weighted.mean(mean,fracM),
            LESm = fracM[which(simp.lith=="Low Energy Siliciclastic")],
            WMh = weighted.mean(mean,fracH),
            LESh = fracH[which(simp.lith=="Low Energy Siliciclastic")])

TestG <- Test %>% ungroup() %>% select(WM,WMm,WMh) %>% gather(key="key",value="value") %>% 
  mutate(key=factor(key, levels = c("WM","WMh","WMm"), labels = c('Low LES Weight', 'High LES Weight', 'Database LES Weight ± ~15%')))

Ms <- TestG %>% group_by(key) %>% summarise(mean=mean(value,na.rm=TRUE)) %>% ungroup()
Md <- tibble(key = c("Unweighted Database Mean","Mean of LES Only"), mean = c(0.67,1.05))
M <- bind_rows(Ms,Md) %>% mutate(key = factor(key, levels = c('Low LES Weight', 'High LES Weight', 'Database LES Weight ± ~15%',"Unweighted Database Mean","Mean of LES Only")))
}
## Plot 
{
ggplot()+
  geom_density(data=TestG,aes(value, fill=key),bw=0.015,alpha=0.8)+
  geom_vline(data=M,aes(xintercept=mean,color= key),linetype='dashed',size=2)+
  scale_color_manual(values = MetBrewer::met.brewer("Hokusai1",n=5))+
  scale_fill_manual(values = MetBrewer::met.brewer("Hokusai1",n=5))+
  labs(x="Database Mean", y = element_blank(), fill = "LES Weighting", color="Distribution and Database Means")+
  lims(x=c(0,1.05))+
  scale_y_continuous(expand=expansion(mult = c(0, .05)))+
  theme_bw()+
  theme(legend.position = c(0.2,0.7))
}


# Fig S9 Bayesian analysis ####
pal <- MetBrewer::met.brewer(name = "Hokusai1")
## Prior (Shape parameters from Poulton et al. (2021)) 
{
a <- 11+2 # number of MIF samples
b <- 36+28-a # Number of MDF samples
o <- seq(from=0,to=1,by=0.01) # Setting up discrete theta values

p <- dbeta(o,a,b) # generating beta distribution for Prior

prior <- data.frame(theta = o,beta=p) # creating a data frame out of theta and betaDist
priMean <- round((a/(a+b)),3)
priMode <- prior$theta[which(prior$beta==max(prior$beta))]

### Panel plot
gPrior <- ggplot(prior,aes(x=theta,y=beta))+
  geom_area(fill=pal[1])+
  geom_vline(xintercept = priMode,linetype='dashed')+
  geom_label(aes(x=0.8,y=max(beta)*0.9), label = paste("Mode = ",priMode))+
  geom_label(aes(x=0.8,y=max(beta)*0.7), label = paste("Mean = ",priMean))+
  theme_ben+
  #labs(title="Prior (beta)",x=expression(theta),y=expression(paste("dbeta(",theta,"|11,25)")))
  labs(title="(A) Prior (beta)",x=expression(theta),y=expression(paste("p(",theta,"|a,b)")))
}
## Likelihood (Shape parameters from Izon and Luo et al. (2022))
{
N <- 60 # Number of data points we have with replicates averaged
z <- 0 # Number of heads (MIF points, or ∆33S>0.4)

D <- data.frame(theta = o) %>%  # Creating a data frame of theta
  mutate(l = (theta^z)*((1-theta)^(N-z))) # Adding column of Bernoulli distribution

Dmean <- mean(D$l)

### Panel plot
gLikelihood <- ggplot(D,aes(x=theta,y=l))+
  geom_area(fill=pal[7])+
  geom_vline(xintercept = z/N,linetype='dashed')+
  geom_label(aes(x=0.8,y=(max(l)*0.9)), label = paste("Mode = ",round((z/N),3)))+
  geom_label(aes(x=0.8,y=max(l)*0.7), label = paste("Mean = ",round(Dmean,3)))+
  theme_ben+
  scale_y_continuous(breaks = pretty(1))+
  labs(title="(B) Likelihood (Bernoulli)",x=expression(theta),y=expression(paste("p(D|",theta,")")))
}

## Posterior (Combining parameters) 
{
pos <- dbeta(o,(z+a),(N-z+b)) # Generating Posterior beta distribution

posterior <- data.frame(theta = o,beta=pos) # Data from of theta and betaDist

posMean <- (z/N)*(N/(N+a+b))+(a/(a+b))*((a+b)/(N+a+b)) # Calculating Posterior Mean

max(posterior$beta) # Max density of posterior to determine 'mode' of theta
posMode <- posterior$theta[which(posterior$beta==max(posterior$beta))]

### Panel plot
gPosterior <- ggplot(posterior,aes(x=theta,y=beta))+
  geom_area(fill=pal[4])+
  geom_vline(xintercept = posMode,linetype='dashed')+
  geom_label(aes(x=0.8,y=max(beta)*0.9), label = paste("Mode = ",posMode))+
  geom_label(aes(x=0.8,y=max(beta)*0.7), label = paste("Mean = ",round(posMean,3)))+
  theme_ben+
  theme(panel.background = element_blank())+
  #labs(title= "Posterior (beta)",x=expression(theta),y=expression(paste("dbeta(",theta,"|0,60)")))
  labs(title= "(C) Posterior (beta)",x=expression(theta),y=expression(paste("p(",theta,"|z,N)")))
}

## Combination figure
gPrior/gLikelihood/gPosterior

