
############################################################################
# Prepare the environment
# ############################################################################

 library(MortCast)
 library(readr)
 library(tidyverse)
 library(hrbrthemes)
 library(cowplot)

source("./functions.R")

############################################################################
# DATA
# ############################################################################

### DATA

POP<-read.table("../data/POP2020b.csv",header=TRUE,fill=TRUE,sep=",")

CoD<-read.table("../data/CoD17to20.csv",header=TRUE,fill=TRUE,sep=",")

# now select the data by year and type (deaths D, Population P and rates r)
# this first just to make the file smaller (too many repetitions in columns)
POP<-POP[,c(2:5,8,12,13)]

#Ages with A99 as the 100+ and TT as the total
Age<-c(as.character(0:99),"A99","TT")
POPb<-POP[(POP$States.and.Territories=="Australia")&(POP$AGE%in%Age),]

# P2020m = population-P year-2020 males-m
P2020m<-POPb[(POPb$Measure=="Population")&(POPb$Time=="2020")&(POPb$SEX_ABS==1),]
P2020f<-POPb[(POPb$Measure=="Population")&(POPb$Time=="2020")&(POPb$SEX_ABS==2),]
D2020m<-POPb[(POPb$Measure=="Deaths")&(POPb$Time=="2020")&(POPb$SEX_ABS==1),]
D2020f<-POPb[(POPb$Measure=="Deaths")&(POPb$Time=="2020")&(POPb$SEX_ABS==2),]

P2019m<-POPb[(POPb$Measure=="Population")&(POPb$Time=="2019")&(POPb$SEX_ABS==1),]
P2019f<-POPb[(POPb$Measure=="Population")&(POPb$Time=="2019")&(POPb$SEX_ABS==2),]
D2019m<-POPb[(POPb$Measure=="Deaths")&(POPb$Time=="2019")&(POPb$SEX_ABS==1),]
D2019f<-POPb[(POPb$Measure=="Deaths")&(POPb$Time=="2019")&(POPb$SEX_ABS==2),]

P2018m<-POPb[(POPb$Measure=="Population")&(POPb$Time=="2018")&(POPb$SEX_ABS==1),]
P2018f<-POPb[(POPb$Measure=="Population")&(POPb$Time=="2018")&(POPb$SEX_ABS==2),]
D2018m<-POPb[(POPb$Measure=="Deaths")&(POPb$Time=="2018")&(POPb$SEX_ABS==1),]
D2018f<-POPb[(POPb$Measure=="Deaths")&(POPb$Time=="2018")&(POPb$SEX_ABS==2),]

P2017m<-POPb[(POPb$Measure=="Population")&(POPb$Time=="2017")&(POPb$SEX_ABS==1),]
P2017f<-POPb[(POPb$Measure=="Population")&(POPb$Time=="2017")&(POPb$SEX_ABS==2),]
D2017m<-POPb[(POPb$Measure=="Deaths")&(POPb$Time=="2017")&(POPb$SEX_ABS==1),]
D2017f<-POPb[(POPb$Measure=="Deaths")&(POPb$Time=="2017")&(POPb$SEX_ABS==2),]

P2016m<-POPb[(POPb$Measure=="Population")&(POPb$Time=="2016")&(POPb$SEX_ABS==1),]
P2016f<-POPb[(POPb$Measure=="Population")&(POPb$Time=="2016")&(POPb$SEX_ABS==2),]
D2016m<-POPb[(POPb$Measure=="Deaths")&(POPb$Time=="2016")&(POPb$SEX_ABS==1),]
D2016f<-POPb[(POPb$Measure=="Deaths")&(POPb$Time=="2016")&(POPb$SEX_ABS==2),]

P2015m<-POPb[(POPb$Measure=="Population")&(POPb$Time=="2015")&(POPb$SEX_ABS==1),]
P2015f<-POPb[(POPb$Measure=="Population")&(POPb$Time=="2015")&(POPb$SEX_ABS==2),]
D2015m<-POPb[(POPb$Measure=="Deaths")&(POPb$Time=="2015")&(POPb$SEX_ABS==1),]
D2015f<-POPb[(POPb$Measure=="Deaths")&(POPb$Time=="2015")&(POPb$SEX_ABS==2),]

#re-arranging so that 100+ is at the end of the vector and total is not included 
P2020m<-P2020m$Value[c(3:102,1)]
P2020f<-P2020f$Value[c(3:102,1)]
P2019m<-P2019m$Value[c(3:102,1)]
P2019f<-P2019f$Value[c(3:102,1)]
P2018m<-P2018m$Value[c(3:102,1)]
P2018f<-P2018f$Value[c(3:102,1)]
P2017m<-P2017m$Value[c(3:102,1)]
P2017f<-P2017f$Value[c(3:102,1)]
P2016m<-P2016m$Value[c(3:102,1)]
P2016f<-P2016f$Value[c(3:102,1)]
P2015m<-P2015m$Value[c(3:102,1)]
P2015f<-P2015f$Value[c(3:102,1)]

D2020m<-D2020m$Value[c(3:102,1)]
D2020f<-D2020f$Value[c(3:102,1)]
D2019m<-D2019m$Value[c(3:102,1)]
D2019f<-D2019f$Value[c(3:102,1)]
D2018m<-D2018m$Value[c(3:102,1)]
D2018f<-D2018f$Value[c(3:102,1)]
D2017m<-D2017m$Value[c(3:102,1)]
D2017f<-D2017f$Value[c(3:102,1)]
D2016m<-D2016m$Value[c(3:102,1)]
D2016f<-D2016f$Value[c(3:102,1)]
D2015m<-D2015m$Value[c(3:102,1)]
D2015f<-D2015f$Value[c(3:102,1)]

##  Kannisto smooth the death rates

## I combine the deaths and populations for the 3 years
# D2019 and P2019 refer to these 3 years

D2019m<-D2019m+D2018m+D2017m
P2019m<-P2019m+P2018m+P2017m
D2019f<-D2019f+D2018f+D2017f
P2019f<-P2019f+P2018f+P2017f

R2020m<-Kannisto(D2020m/P2020m)
R2020f<-Kannisto(D2020f/P2020f)
R2019m<-Kannisto(D2019m/P2019m)
R2019f<-Kannisto(D2019f/P2019f)
R2018m<-Kannisto(D2018m/P2018m)
R2018f<-Kannisto(D2018f/P2018f)
R2017m<-Kannisto(D2017m/P2017m)
R2017f<-Kannisto(D2017f/P2017f)
R2016m<-Kannisto(D2016m/P2016m)
R2016f<-Kannisto(D2016f/P2016f)
R2015m<-Kannisto(D2015m/P2015m)
R2015f<-Kannisto(D2015f/P2015f)

############################################################################
# Life tables
# ############################################################################

## life tables including CI
## LT20m = life table-LT year-2020 males-m
LT20m<-lifetable.mx(R2020m,"m")
CI20m<-unlist(LTci(UnGroup(D2020m)[1:111],R2020m,"m"))

LT20f<-lifetable.mx(R2020f,"f")
CI20f<-unlist(LTci(UnGroup(D2020f)[1:111],R2020f,"f"))

LT19m<-lifetable.mx(R2019m,"m")
CI19m<-unlist(LTci(UnGroup(D2019m)[1:111],R2019m,"m"))

LT19f<-lifetable.mx(R2019f,"f")
CI19f<-unlist(LTci(UnGroup(D2019f)[1:111],R2019f,"f"))

LT15m<-lifetable.mx(R2015m,"m")
LT15f<-lifetable.mx(R2015f,"f")

CI20me60<-unlist(LTcie60(UnGroup(D2020m)[1:111],R2020m,"m"))
CI20fe60<-unlist(LTcie60(UnGroup(D2020f)[1:111],R2020f,"f"))
CI19me60<-unlist(LTcie60(UnGroup(D2019m)[1:111],R2019m,"m"))
CI19fe60<-unlist(LTcie60(UnGroup(D2019f)[1:111],R2019f,"f"))
CI15me60<-unlist(LTcie60(UnGroup(D2015m)[1:111],R2015m,"m"))
CI15fe60<-unlist(LTcie60(UnGroup(D2015f)[1:111],R2015f,"f"))
 
# the Tables area arranged by value, followed by the ci-L low and then ci-H high
# first for females and then for males
Table1<-round(   cbind(
  rbind(c(LT20f[1,10],CI20f[c(1,3)]),
        c(LT19f[1,10],CI19f[c(1,3)])),
  rbind(c(LT20m[1,10],CI20m[c(1,3)]),
        c(LT19m[1,10],CI19m[c(1,3)])))
  ,2)

Table1e60<-round(   cbind(
  rbind(c(LT20f[61,10],CI20fe60[c(1,3)]),
        c(LT19f[61,10],CI19fe60[c(1,3)])),
  rbind(c(LT20m[61,10],CI20me60[c(1,3)]),
        c(LT19m[61,10],CI19me60[c(1,3)])))
  ,2)

############################################################################
# Differences
# ############################################################################

Dif20m<-cbind(LT20m[1,10]-LT19m[1,10],(LT19m[1,10]-LT15m[1,10])/4)
CID20m<-unlist(LTci2(UnGroup(D2020m)[1:111],R2020m,UnGroup(D2019m)[1:111],R2019m,"m"))
CID19m<-unlist(LTci2(UnGroup(D2019m)[1:111],R2019m,UnGroup(D2015m)[1:111],R2015m,"m")/4)
Dif20f<-cbind(LT20f[1,10]-LT19f[1,10],(LT19f[1,10]-LT15f[1,10])/4)
CID20f<-unlist(LTci2(UnGroup(D2020f)[1:111],R2020f,UnGroup(D2019f)[1:111],R2019f,"f"))
CID19f<-unlist(LTci2(UnGroup(D2019f)[1:111],R2019f,UnGroup(D2015f)[1:111],R2015f,"f")/4)
# now for e60
Dif20me60<-cbind(LT20m[61,10]-LT19m[61,10],(LT19m[61,10]-LT15m[61,10])/4)
CID20me60<-unlist(LTci2b(UnGroup(D2020m)[1:111],R2020m,UnGroup(D2019m)[1:111],R2019m,"m"))
CID19me60<-unlist(LTci2b(UnGroup(D2019m)[1:111],R2019m,UnGroup(D2015m)[1:111],R2015m,"m")/4)
Dif20fe60<-cbind(LT20f[61,10]-LT19f[61,10],(LT19f[61,10]-LT15f[61,10])/4)
CID20fe60<-unlist(LTci2b(UnGroup(D2020f)[1:111],R2020f,UnGroup(D2019f)[1:111],R2019f,"f"))
CID19fe60<-unlist(LTci2b(UnGroup(D2019f)[1:111],R2019f,UnGroup(D2015f)[1:111],R2015f,"f")/4)

Table2<-round(   
  c(Dif20f[1],CID20f[c(1,3)],Dif20m[1],CID20m[c(1,3)])
  ,2)

Table2e60<-round(   
  c(Dif20fe60[1],CID20fe60[c(1,3)],Dif20me60[1],CID20me60[c(1,3)])
  ,2)

############################################################################
# Age contribution
# ############################################################################

## age decomposition including CI
# males
cbind(LT20m[1,10]-LT19m[1,10],(LT19m[1,10]-LT15m[1,10])/4)
# 2019 to 2020
A<-AgeDecomp(LT19m,LT20m)
# age-groups = 0-59, 60-79, 80+
Agm<-c(sum(A[1:60]),sum(A[61:80]),sum(A[81:111]))
# and their CI
CIAgm<-t(matrix(unlist(LTci2c(UnGroup(D2019m)[1:111],R2019m,UnGroup(D2020m)[1:111],R2020m,"m")),3))

# now for females
cbind(LT20f[1,10]-LT19f[1,10],(LT19f[1,10]-LT15f[1,10])/4)
A<-AgeDecomp(LT19f,LT20f)
Agf<-c(sum(A[1:60]),sum(A[61:80]),sum(A[81:111]))
CIAgf<-t(matrix(unlist(LTci2c(UnGroup(D2019f)[1:111],R2019f,UnGroup(D2020f)[1:111],R2020f,"f")),3))


Table3<-round(   
  cbind(Agf,CIAgf[,c(1,3)],Agm,CIAgm[,c(1,3)])
  ,2)

############################################################################
# Cause of death decomposition
# ############################################################################

# preparing the causes of death data

CoD17m<-CoD[1:12,2:9]
CoD18m<-CoD[1:12,10:17]
CoD19m<-CoD[1:12,18:25]
CoD20m<-CoD[1:12,26:33]
CoD17f<-CoD[14:25,2:9]
CoD18f<-CoD[14:25,10:17]
CoD19f<-CoD[14:25,18:25]
CoD20f<-CoD[14:25,26:33]

CoDABS<-function(Mm){
MM<-matrix(as.numeric(unlist(Mm[,-1])),12)
mm<-MM[-1,-7]
CauseT<-MM[1,-7]
AgeT<-MM[-1,7]
CauseO<-colSums(mm)
AgeO<-rowSums(mm)
TT<-MM[1,7]
mm2<-mm+(matrix(rep((CauseT-CauseO),each=11),11)*matrix(rep((AgeT-AgeO)/(TT-sum(mm)),6),11))
mm3<-mm2/matrix(rep(rowSums(mm2),6),11)
return(mm3)}

CoD2<-function(Mm1){
  MM<-matrix(as.numeric(unlist(Mm[,-1])),12)
  mm<-MM[-1,-7]
  return(mm)}

CoDABS3<-function(Mm1,Mm2,Mm3){
    
  MM1<-matrix(as.numeric(unlist(Mm1[,-1])),12)
  MM2<-matrix(as.numeric(unlist(Mm2[,-1])),12)
  MM3<-matrix(as.numeric(unlist(Mm3[,-1])),12)
  mm<-MM1[-1,-7]+MM2[-1,-7]+MM3[-1,-7]
  CauseT<-MM1[1,-7]+MM2[1,-7]+MM3[1,-7]
  AgeT<-MM1[-1,7]+MM2[-1,7]+MM3[-1,7]
  CauseO<-colSums(mm)
  AgeO<-rowSums(mm)
  TT<-MM1[1,7]+MM2[1,7]+MM3[1,7]
  mm2<-mm+(matrix(rep((CauseT-CauseO),each=11),11)*matrix(rep((AgeT-AgeO)/(TT-sum(mm)),6),11))
  mm3<-mm2/matrix(rep(rowSums(mm2),6),11)
  return(mm3)}


IICD19m<-CoDABS3(CoD17m,CoD18m,CoD19m)
IICD19f<-CoDABS3(CoD17f,CoD18f,CoD19f)

## cause decomposition including CI
# Males 2019 to 2020
CauseDm<-matrix(unlist(LTci2e(UnGroup(D2019m)[1:111],R2019m,IICD19m,UnGroup(D2020m)[1:111],R2020m,CoDABS(CoD20m),"m")),6)

# Females 2019 to 2020
CauseDf<-matrix(unlist(LTci2e(UnGroup(D2019f)[1:111],R2019f,IICD19f,UnGroup(D2020f)[1:111],R2020f,CoDABS(CoD20f),"f")),6)

Table5<-round(   
  cbind(CauseDf[,2],CauseDf[,c(1,3)],CauseDm[,2],CauseDm[,c(1,3)])
  ,2)

############################################################################
# Prep Aburto et al. data
# ############################################################################

# Prep the Aburto data for 2017-19 average and using open age group 100+

### NB: requires downloading this data and placing it in the "data folder", see README for details

df.lt.prep <- read_rds("../data/lt_input.rds")
df.lt.prep.dk.us <- filter(df.lt.prep, (region_iso == "DK" | region_iso == "US") & (year %in% 2015:2020 & year != 2016))
# Mx rates
df.lt.prep$mx <- df.lt.prep$death_total / df.lt.prep$population_py

# Aburto et al. LT function
CalculateLifeTable <-
  function (df, x, nx, Dx, Ex) {

    require(dplyr)

    df %>%
      transmute(
        x = {{x}},
        nx = {{nx}},
        mx = {{Dx}}/{{Ex}},
        px = exp(-mx*{{nx}}),
        qx = 1-px,
        lx = head(cumprod(c(1, px)), -1),
        dx = c(-diff(lx), tail(lx, 1)),
        Lx = ifelse(mx==0, lx*nx, dx/mx),
        Tx = rev(cumsum(rev(Lx))),
        ex = Tx/lx
      )

  }

# Get the average mx rates 2017-2019
df.lt.prep.1719 <- df.lt.prep.dk.us %>%
  filter(year %in% 2017:2019) %>%
  arrange(region_iso, sex, year, age_start) %>%
  group_by(region_iso, sex, age_start) %>%
  mutate(meanmx = mean(death_total / population_py), 
    totmx = sum(death_total) / sum(population_py), 
    totmx2 = mean(death_total) / mean(population_py), 
    totdx = sum(death_total),
    totpop = sum(population_py)) %>%
  ungroup()
  
  # Grab just one year since they're the same
  df.lt.prep.1719.r <- df.lt.prep.1719 %>% filter(year == 2017)

# Bring them all together
df.lt.prep.1520 <- df.lt.prep.dk.us %>%
  filter(year == 2015 | year == 2020)

df.lt.prep.avg <- df.lt.prep.1719.r %>% 
  rename(population_py_old = population_py, death_total_old = death_total) %>%
  mutate(death_total = totdx, population_py = totpop) %>%
  select(-c(meanmx, totmx, totmx2, death_total_old, population_py_old, totdx, totpop)) 

df.dk.us <- bind_rows(df.lt.prep.1520, df.lt.prep.avg)

# open age_group 100+
lt_100 <-
  df.dk.us %>%
  arrange(region_iso, sex, year, age_start) %>%
  group_by(region_iso, sex, year) %>%
  group_modify(~{
    CalculateLifeTable(.x, age_start, age_width, death_total, population_py)
  }) %>%
  ungroup()

# CI
n_sim = 500
lt_100_sim <- df.dk.us %>%
  expand_grid(id_sim = 1:n_sim) %>%
  group_by(region_iso, sex, year, age_start) %>%
  mutate(death_total_sim = rpois(n_sim, death_total)) %>%
  arrange(region_iso, sex, year, age_start) %>%
  group_by(id_sim, region_iso, sex, year) %>%
  group_modify(~{
    CalculateLifeTable(.x, age_start, age_width, death_total_sim, population_py)
  }) %>%
  ungroup()

# Difference
lt_ex_diff_mean <- 
  lt_100 %>%
  # filter(year %in% c(2017:2020)) %>%
  select(region_iso, sex, year, x, mx, ex) %>%
  arrange(region_iso, sex, x, year) %>%
  group_by(region_iso, sex, x) %>%
  mutate(
    # annual ex difference (delta ex_y = ex_{y+1} - ex_y)
    ex_diff = c(diff(ex), NA)
    # average annual ex difference 2015 to 2019
  ) %>%
  ungroup()

# Difference CI
lt_ex_diff_ci <-
  lt_100_sim %>%
  # filter(year %in% c(2017:2020)) %>%
  select(id_sim, region_iso, sex, year, x, mx, ex) %>%
  arrange(id_sim, region_iso, sex, x, year) %>%
  group_by(id_sim, region_iso, sex, x) %>%
  mutate(
    ex_diff = c(diff(ex), NA)
  ) %>%
  group_by(region_iso, sex, x, year) %>%
  summarise(
    ex_q025 = quantile(ex, 0.025, na.rm = TRUE),
    ex_q975 = quantile(ex, 0.975, na.rm = TRUE),
    ex_diff_q025 = quantile(ex_diff, 0.025, na.rm = TRUE),
    ex_diff_q975 = quantile(ex_diff, 0.975, na.rm = TRUE)
  ) %>%
  ungroup()

# Prep for plotting
lt_ex_diff_mean.sub <- lt_ex_diff_mean %>% filter(x == 0 | x == 60) %>% select(-c("mx"))
lt_ex_diff_ci.sub <- lt_ex_diff_ci %>% filter(x == 0 | x == 60)

lt_dk_us <- left_join(lt_ex_diff_mean.sub, lt_ex_diff_ci.sub)
lt_dk_us$name <- ifelse(lt_dk_us$region_iso == "DK", "Denmark", "United States")
lt_dk_us["year"][lt_dk_us["year"] == 2017] <- 2019

############################################################################
# Figure 1
# ############################################################################

# AU results
e0.20.f <- data.frame(name = "Australia"
  , sex = "Female"
  , year = 2020
  , x = 0
  , ex = LT20f[1,10])
e0.19.f <- data.frame(name = "Australia"
  , sex = "Female"
  , year = 2019
  , x = 0
  , ex = LT19f[1,10])
e0.15.f <- data.frame(name = "Australia"
  , sex = "Female"
  , year = 2015
  , x = 0
  , ex = LT15f[1,10])
e0.20.m <- data.frame(name = "Australia"
  , sex = "Male"
  , year = 2020
  , x = 0
  , ex = LT20m[1,10])
e0.19.m <- data.frame(name = "Australia"
  , sex = "Male"
  , year = 2019
  , x = 0
  , ex = LT19m[1,10])
e0.15.m <- data.frame(name = "Australia"
  , sex = "Male"
  , year = 2015
  , x = 0
  , ex = LT15m[1,10])
e60.20.f <- data.frame(name = "Australia"
  , sex = "Female"
  , year = 2020
  , x = 60
  , ex = LT20f[61,10])
e60.19.f <- data.frame(name = "Australia"
  , sex = "Female"
  , year = 2019
  , x = 60
  , ex = LT19f[61,10])
e60.15.f <- data.frame(name = "Australia"
  , sex = "Female"
  , year = 2015
  , x = 60
  , ex = LT15f[61,10])
e60.20.m <- data.frame(name = "Australia"
  , sex = "Male"
  , year = 2020
  , x = 60
  , ex = LT20m[61,10])
e60.19.m <- data.frame(name = "Australia"
  , sex = "Male"
  , year = 2019
  , x = 60
  , ex = LT19m[61,10])
e60.15.m <- data.frame(name = "Australia"
  , sex = "Male"
  , year = 2015
  , x = 60
  , ex = LT15m[61,10])

p1 <- bind_rows(lt_dk_us, 
  e0.20.f, 
  e0.19.f, 
  e0.15.f, 
  e0.20.m, 
  e0.19.m, 
  e0.15.m, 
  e60.20.f, 
  e60.19.f, 
  e60.15.f, 
  e60.20.m, 
  e60.19.m, 
  e60.15.m
  )

p1$year <- as.factor(p1$year)
p1 <- p1 %>%
mutate(age = x %>% as_factor())
p1$name <- ordered(p1$name, levels = c("United States", "Denmark", "Australia"))

age.labs <- c("Age 0", "Age 60")
names(age.labs) <- c("0", "60")

ggplot(p1, aes(ex, name, color = name, shape = year)) +
  geom_point() + 
  facet_grid(sex ~ age, scales = "free_x")

sex.labs <- c("Females", "Males")
names(sex.labs) <- c("Female", "Male")

ggplot(p1, aes(year, ex, color = name)) +
  geom_point() + 
  geom_line(aes(linetype = name, group = name, color = name)) + 
  facet_grid(age ~ sex, scales = "free_y", labeller = labeller(age = age.labs, sex = sex.labs)) + 
  scale_linetype_manual(name = "", values = c("dotdash", "longdash", "solid")) + 
  scale_color_manual(name = "", values = c("#B5223BFF", "#64B6EEFF", "#38006b")) +
  scale_x_discrete(breaks = c("2015", "2019", "2020"), labels = c("2015", "2017-19", "2020")) +
  theme_minimal(base_family = font_rc) +
  theme(
      legend.position = "bottom",
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
        strip.text = element_text(face = 2),
      panel.spacing.x = unit(2, "lines"),
      panel.spacing.y = unit(2, "lines"),
      axis.text.y = element_text(face = 2),
      legend.key.width = unit(3,"line"),
     plot.background = element_rect(color = "black", fill = "white")
  ) +
  labs(
      y = "Life expectancy, years",
      x = "Time periods"
  )
  one <- last_plot()
  ggsave("../output/fig-1.pdf", width = 6, height = 4.5, device = cairo_pdf)

############################################################################
# Figure 2
# ############################################################################

e0.diff.f <- data.frame(name = "Australia"
  , year = 2019
  , sex = "Female"
  , x = 0
  , ex_diff = Table2[1]
  , ex_diff_q025 = Table2[2]
  , ex_diff_q975 = Table2[3]
)
e60.diff.f <- data.frame(name = "Australia"
  , year = 2019
  , sex = "Female"
  , x = 60
  , ex_diff = Table2e60[1]
  , ex_diff_q025 = Table2e60[2]
  , ex_diff_q975 = Table2e60[3]
)
e0.diff.m <- data.frame(name = "Australia"
  , year = 2019
  , sex = "Male"
  , x = 0
  , ex_diff = Table2[4]
  , ex_diff_q025 = Table2[5]
  , ex_diff_q975 = Table2[6]
)
e60.diff.m <- data.frame(name = "Australia"
  , year = 2019
  , sex = "Male"
  , x = 60
  , ex_diff = Table2e60[4]
  , ex_diff_q025 = Table2e60[5]
  , ex_diff_q975 = Table2e60[6]
)

lt_dk_us_change <- filter(lt_dk_us, year == 2019)

p2 <- bind_rows(lt_dk_us_change, 
        e0.diff.f ,
        e60.diff.f ,
        e0.diff.m ,
        e60.diff.m
  )

p2 <- p2 %>%
mutate(age = x %>% as_factor())
p2$name <- ordered(p2$name, levels = c("United States", "Denmark", "Australia"))

ggplot(p2, aes(x = name)) +
    geom_pointrange(
        aes(
            color = name,
            ymin = ex_diff_q025,
            y = ex_diff,
            ymax = ex_diff_q975
        ),
        fatten = .7,
        size = .3
    ) +
    geom_hline(yintercept = 0, size = 2, color = "#bababa") +
  facet_grid(age ~ sex, scales = "free_y", labeller = labeller(age = age.labs, sex = sex.labs)) + 
  scale_color_manual(name = "", values = c("#B5223BFF", "#64B6EEFF", "#38006b")) +
      scale_y_continuous(breaks = seq(-2, 1.5, .5), expand = c(0,0))+
    coord_cartesian(ylim = c(-2.5, 1.0))+
  theme_minimal(base_family = font_rc) +
  theme(
      legend.position = "none",
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
        strip.text = element_text(face = 2),
      panel.spacing.x = unit(2, "lines"),
      panel.spacing.y = unit(2, "lines"),
      axis.text.y = element_text(face = 2),
      legend.key.width = unit(3,"line")
  ) +
  labs(
      y = "Difference in life expectancy, years",
      x = ""
  )
  two <- last_plot()
  ggsave("../output/fig-2.pdf", width = 6, height = 4.5, device = cairo_pdf)

############################################################################
# Figure 3
# ############################################################################

# Bring in HMD data for AU
Af<-read.table("../data/fltper_1x1.txt",header=TRUE,fill=TRUE,skip=1)
Am<-read.table("../data/mltper_1x1.txt",header=TRUE,fill=TRUE,skip=1)

FF<-c(Af[Af$Age==0,]$ex,LT19f[1,10])
MM<-c(Am[Am$Age==0,]$ex,LT19m[1,10])

c(diff(FF), NA)

nn<-length(FF)
YY<-1922:2019
count<-FF[-1]-FF[-nn]
names(count)<-YY
female.e0.diff <- data.frame(YY, count, sex = "Females")
count<-MM[-1]-MM[-nn]
names(count)<-YY
male.e0.diff <- data.frame(YY, count, sex = "Males")

e0.diff <- rbind(female.e0.diff, male.e0.diff)

df20 <- p2 %>% filter(age == 0 & name == "Australia") %>%
  select(name, sex, ex_diff) %>%
  mutate(YY = 2020)

  df20$sex <- factor(df20$sex, levels = c("Female", "Male"), labels = c("Females", "Males"))

ggplot(df20)+
    geom_hline(
        aes(yintercept = 0), color = "#ffea00", size = .5
    )+
    geom_col(aes(YY, ex_diff), fill = "#64B6EEFF", width = 1.5)+
    geom_col(
      data = e0.diff,
        # data = e0.diff %>% filter(sex == "Male"),
        aes(YY, count),fill = "#777777", width = 1
    )+
    geom_segment(
        aes(y = ex_diff, yend = ex_diff, x = 1921, xend = 2020),
        color = "#64B6EEFF", size = .3
    )+
    facet_grid(.~sex) + 
    scale_x_continuous(
        breaks = c(1922, 1939, 1945, 1965, 1990, 2014),
        labels = c("1922", "", "'45", "'65", "'90", "2014")
    )+
    scale_y_continuous(breaks = seq(-4, 4, 2), position = "right")+

    coord_cartesian(ylim = c(-3, 3), expand = F)+
    theme_minimal(base_family = font_rc)+
    theme(
        panel.grid.minor = element_blank(),
        panel.spacing.x = unit(1, "lines"),
        strip.text = element_text(face = 2),
        strip.background = element_blank(),
        legend.position = c(.88, .04),
        plot.title = element_text(family = "Roboto Slab")
    )+
    labs(
        x = NULL,
        y = "Yearly change in life expectancy at birth",
        fill = "Change e0, years"
    )+
    geom_text(
        data = tibble(YY = 2000, ex_diff = 1),
        aes(YY, ex_diff),
        label = "Change in 2020 vs 2017-19", size = 5, family = font_rc,
        color = "#64B6EEFF", fontface = 2
    )

three <- last_plot()
ggsave("../output/fig-3.pdf", width = 6, height = 4.5, device = cairo_pdf)

############################################################################
# Figure 4
# ############################################################################

age.m.1920.df <- data.frame(sex = "Males"
  , ctb = Agm
  , age3 = c("0-59", "60-79", "80+")
  , period = "2017-2019 to 2020")

age.f.1920.df <- data.frame(sex = "Females"
  , ctb = Agf
  , age3 = c("0-59", "60-79", "80+")
  , period = "2017-2019 to 2020")

df.age <- rbind(
  age.m.1920.df
  , age.f.1920.df
)

ggplot(df.age, aes(ctb, age3, fill = period)) +
      geom_col()+
    geom_vline(xintercept = 0, size = .25, color = "#18ffff")+
    facet_grid(.~sex) + 
    scale_fill_manual(
        values = c("#38006b"),
        guide  = guide_legend(ncol = 1, reverse = TRUE)
    )+
    coord_cartesian(xlim = c(-.05, 1), expand = F)+
    scale_x_continuous(
        breaks = c(-0.5, 0, .5, 1)
    )+
    theme_minimal(base_family = font_rc)+
    theme(
        panel.grid.minor.y = element_blank(),
        panel.spacing = unit(.5, "lines"),
        strip.text = element_text(face = 2),
        legend.position = c(.93, .1)
    )+
    labs(
        fill = "Time Period",
        x = "Losses|Gains in life expectancy at birth, years",
        y = "Age groups"
    )

four_f <- last_plot()

ggsave("../output/fig-4.pdf", width = 7, height = 4.5, device = cairo_pdf)

############################################################################
# Figure 5
# ############################################################################

cause.m.df <- data.frame(sex = "Males"
  , causegroups = c("Neoplasms", "CVD", "Respiratory", "External", "COVID-19", "Others")
  , ctb = Table5[,4])
cause.f.df <- data.frame(sex = "Females"
  , causegroups = c("Neoplasms", "CVD", "Respiratory", "External", "COVID-19", "Others")
  , ctb = Table5[,1])

cause.df <- rbind(cause.m.df, cause.f.df)

cause.df$causegroups <- factor(cause.df$causegroups,
  levels = c("COVID-19", "Neoplasms", "CVD", "Respiratory", "External", "Others"),
  ordered = TRUE)

ggplot(cause.df, aes(ctb, sex, fill = causegroups)) +
  geom_col(position = position_stack(reverse = TRUE)) +
    geom_vline(xintercept = 0, size = .25, color = "#18ffff")+
    coord_cartesian(xlim = c(-.05, .8), expand = F)+
    # scale_fill_brewer(palette = "Set2") + 
    scale_fill_ipsum() +
    theme_minimal(base_family = font_rc)+
    theme(
        legend.position = "bottom",
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.spacing.x = unit(2, "lines"),
        strip.text = element_blank(),
        axis.text.y = element_text(face = 2),
    )+
    labs(
        fill = "Cause of death",
        x = "Losses|Gains in life expectancy at birth, years",
        y = NULL
    ) + guides(fill = guide_legend(nrow = 1))

five <- last_plot()

ggsave("../output/fig-5.pdf", width = 8, height = 4.5, device = cairo_pdf)

############################################################################
# Combined graph
# ############################################################################

p1.grid <- plot_grid(one, two, labels = c('A', 'B'))
p2.grid <- plot_grid(four_f, five, labels = c('D', 'E'))
plot_grid(p1.grid, three, p2.grid, ncol = 1, labels = c('', 'C', ''))

ggsave("../output/fig-combined.pdf", width = 16, height = 12, device = cairo_pdf)
