library("ggplot2")

input_data = "C:\\Users\\guest\\Downloads\\advanced_retail_sales.csv"
temp_df = read.csv(input_data)
temp_df[,"DATE"] = as.Date(temp_df[,"DATE"], format="%m/%d/%Y")


#differences 
num_of_obs = length(temp_df[,"RSXFSN"])
period_diff = 1

#first differences
for(t in 1:num_of_obs)
{
  temp_df[t+period_diff,"first_diff"] = temp_df[t+period_diff,"RSXFSN"]-temp_df[t,"RSXFSN"]
}

#lag first difference
for(t in 1:num_of_obs)
{
  temp_df[t+1, "lag_first_diff"] = temp_df[t, "first_diff"]
}

#create ARI(1,11)
ari_lm = lm(first_diff~lag_first_diff, temp_df)
summary(ari_lm)
temp_co = coefficients(ari_lm)

#add forecast to model 
for(t in 1:(num_of_obs+1))
{
  temp_df[t,"ari_forecast"] = temp_co[1]+temp_co[2]*temp_df[t,"lag_first_diff"]
  temp_df[t,"ari_residual"] = temp_df[t,"first_diff"] - temp_df[t,"ari_forecast"]
}

#lag ari_residual
for(t in 1:num_of_obs)
{
  temp_df[t+1, "lag_ari_residual"] = temp_df[t, "ari_residual"]
}

#create ARIMA(1,1,1)
arima_lm = lm(first_diff~lag_ari_residual+ari_forecast, temp_df)
summary(arima_lm)
arima_co = coefficients(arima_lm)

#add arima(1,1,1) forecast
for(t in 1:(num_of_obs+1))
{
  temp_df[t,"arima_forecast"] = arima_co[1]+arima_co[2]*temp_df[t,"lag_ari_residual"]+arima_co[2]*temp_df[t,"ari_forecast"]
  temp_df[t,"arima_residual"] = temp_df[t,"first_diff"] - temp_df[t,"arima_forecast"]
}

#retail sales forecast 
for(t in 1:num_of_obs)
{
  temp_df[t+1,"ari_rsxfsn"] = temp_df[t+1,"ari_forecast"] + temp_df[t,"RSXFSN"]
  temp_df[t+1,"arima_rsxfsn"] = temp_df[t+1,"arima_forecast"] + temp_df[t,"RSXFSN"]
}

#plot 
p = ggplot(temp_df)
p = p + geom_line(aes(x=DATE,y=RSXFSN), color="darkgrey", size=0.75)
p = p + geom_point(aes(x=DATE,y=RSXFSN), color="darkmagenta", size=1)
p = p + geom_line(aes(x=DATE,y=ari_rsxfsn), color="blue", size=0.75)
p = p + geom_line(aes(x=DATE,y=arima_rsxfsn), color="red", size=0.75)

















