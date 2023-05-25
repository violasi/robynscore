# Import needed library
```
library(robynscore)
library(Robyn)
library(doParallel)
library(foreach)
library(stringr)

library(data.table)
library(ggplot2)
library(prophet)
library(lubridate)
library(dplyr)
library(openxlsx)
library(caroline)


```

# Import Robyn model
```
data(Robyn.RData) #Robyn output data
robyn_model<- list(
  dt_orig = data.frame(Robyn$listInit$InputCollect$dt_input), # input data, change dataset here if more data is available
  media_all = c("tv_S","ooh_S","print_S","facebook_I","search_clicks_P","newsletter"), # media names
  channel_group = data.frame(media = c("tv_S","ooh_S","print_S","facebook_I","search_clicks_P","newsletter"),media_group=c("offline","offline","offline","online","online","online")), # media group, can be null
  date_var = "DATE", # date column
  dep_var = "revenue", # y column
  adstock = "geometric", # geometric, weibull_cdf or weibull_pdf
  alphas = unlist(Robyn$listInit$OutputCollect$resultHypParam)[
    grepl("_alphas",names(Robyn$listInit$OutputCollect$resultHypParam))], # named vector with media names + _alphas
  gammas = unlist(Robyn$listInit$OutputCollect$resultHypParam)[
    grepl("_gammas",names(Robyn$listInit$OutputCollect$resultHypParam))], # named vector with media names +  _gammas
  thetas = unlist(Robyn$listInit$OutputCollect$resultHypParam)[
    grepl("_thetas",names(Robyn$listInit$OutputCollect$resultHypParam))], # named vector with media names + _thetas,if geometric
  coefs = nv(Robyn$listInit$OutputCollect$xDecompAgg$coef,
             Robyn$listInit$OutputCollect$xDecompAgg$rn), # coefs vector with media names
  model_start = as.Date("2016-11-21"), #date model window start
  model_end = as.Date("2018-08-20"), #date model window end
  holidays = subset(Robyn$listInit$InputCollect$dt_holidays,country=="DE"), #holiday dataframe used
  prophet_var = c("trend","season","holiday"),#prophet var used
  prophet_start = as.Date("2015-11-23"),# date used to fit prophet
  prophet_end = as.Date("2019-11-11")
)
```

# deep dive into "facebook_I"
```
media_deep_dive<-media_result_all(media = "facebook_I",
                                  model_input = robyn_model)
plot(media_deep_dive$spd_adstock,media_deep_dive$response_daily) #saturation curve
```

# response, roi, and mroi during 2018 December
```
campaign<-c(as.Date("2018-12-1"),as.Date("2018-12-31"))

media_response(media = "facebook_I",
               model_input = robyn_model,
               promo_range = campaign)

change.f.media(media = "facebook_I",
               model_input = robyn_model,
               promo_range = campaign)

media_spd(media = "facebook_I",
               model_input = robyn_model,
               promo_range = campaign)

media_response(media = "facebook_I",
               model_input = robyn_model,
               promo_range = campaign)/
  media_spd(media = "facebook_I",
            model_input = robyn_model,
            promo_range = campaign)
            
```
# media summary
```
orga_df <- organic_df(robyn_model)

media_need<-robyn_model$media_all
summary<-media_summary(media_need,robyn_model,df_orga=orga_df,
                     promo_range =campaign)
channel_group_summary(summary)
```
# daily mroi curve
```
roi_daily <- curveDaily(media_need,robyn_model)
plot(roi_daily$spd_adstock[roi_daily$media=="facebook_I"],roi_daily$response[roi_daily$media=="facebook_I"])
plot(roi_daily$spd_adstock[roi_daily$media=="facebook_I"],roi_daily$roi.m[roi_daily$media=="facebook_I"])
```
# campaign mroi curve
```
media_need<-c("tv_S"   ,         "ooh_S"    ,       "print_S")
roi_campaign <- curveCampaign(media_need,robyn_model,campaign)
summary<-media_summary(media_need,robyn_model,promo_range =campaign)
ggplot(subset(roi_campaign,media %in% media_need),aes(x=spd,y=roi.m,colour=media))+
  geom_line()+
  geom_point(aes(x=summary$spd[1],
                 y= summary$roi.m[1]),
             color='red',shape = 2,
             size=2)+
  geom_point(aes(x=summary$spd[2],
                 y= summary$roi.m[2]),
             color='red',shape = 2,
             size=2)+
  geom_point(aes(x=summary$spd[3],
                 y= summary$roi.m[3]),
             color='red',shape = 2,
             size=2)
```
# budget allocation
```
budget1<-budget_spd_unchange(media_need,robyn_model,promo_range =campaign)
budget1$new_spd/budget1$old_spd
budget1$new_np/budget1$old_np
summary_after<-media_summary(media_need,robyn_model,promo_range =campaign,new_spd=budget1$new_spd)
ggplot(subset(roi_campaign,media %in% media_need),aes(x=spd,y=roi.m,colour=media))+
  geom_line()+
  geom_point(aes(x=summary$spd[1],
                 y= summary$roi.m[1]),
             color='red',shape = 2,
             size=2)+
  geom_point(aes(x=summary$spd[2],
                 y= summary$roi.m[2]),
             color='red',shape = 2,
             size=2)+
  geom_point(aes(x=summary$spd[3],
                 y= summary$roi.m[3]),
             color='red',shape = 2,
             size=2)+
  geom_point(aes(x=summary_after$spd[1],
                 y= summary_after$roi.m[1]),
             color='green',shape = 2,
             size=2)+
  geom_point(aes(x=summary_after$spd[2],
                 y= summary_after$roi.m[2]),
             color='green',shape = 2,
             size=2)+
  geom_point(aes(x=summary_after$spd[3],
                 y= summary_after$roi.m[3]),
             color='green',shape = 2,
             size=2)


budget2<-budget_spd_tgt(media_need,robyn_model,promo_range =campaign,old_spd=budget1$new_spd,
                        tgt=960000)
budget2$new_spd/budget2$old_spd
budget2$new_np/budget2$old_np
budget2$new_np/960000
summary_after2<-media_summary(media_need,robyn_model,promo_range =campaign,new_spd=budget2$new_spd)
ggplot(subset(roi_campaign,media %in% media_need),aes(x=spd,y=roi.m,colour=media))+
  geom_line()+
  geom_point(aes(x=summary$spd[1],
                 y= summary$roi.m[1]),
             color='red',shape = 2,
             size=2)+
  geom_point(aes(x=summary$spd[2],
                 y= summary$roi.m[2]),
             color='red',shape = 2,
             size=2)+
  geom_point(aes(x=summary$spd[3],
                 y= summary$roi.m[3]),
             color='red',shape = 2,
             size=2)+
  geom_point(aes(x=summary_after$spd[1],
                 y= summary_after$roi.m[1]),
             color='green',shape = 2,
             size=2)+
  geom_point(aes(x=summary_after$spd[2],
                 y= summary_after$roi.m[2]),
             color='green',shape = 2,
             size=2)+
  geom_point(aes(x=summary_after$spd[3],
                 y= summary_after$roi.m[3]),
             color='green',shape = 2,
             size=2)+
  geom_point(aes(x=summary_after2$spd[1],
                 y= summary_after2$roi.m[1]),
             color='blue',shape = 2,
             size=2)+
  geom_point(aes(x=summary_after2$spd[2],
                 y= summary_after2$roi.m[2]),
             color='blue',shape = 2,
             size=2)+
  geom_point(aes(x=summary_after2$spd[3],
                 y= summary_after2$roi.m[3]),
             color='blue',shape = 2,
             size=2)
```
