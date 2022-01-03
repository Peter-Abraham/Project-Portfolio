######## Auto Regression #########
#data manipulation
input_data = "\\Users\\peterabraham\\Downloads\\advanced_retail_sales.csv"
temp_df = read.csv(input_data)
num_of_obs = length(temp_df[,"DATE"])

#create lag 1 
for(t in 1:num_of_obs)
{
  temp_df[t+1,"lag_1"] = temp_df[t,"RSXFSN"]
}


#building the linear model 
temp_lm = lm(RSXFSN~lag_1,temp_df)
b0 = coefficients(temp_lm)[1]
b1 = coefficients(temp_lm)[2]

#create function to add forecast, residuals, residuals sqaured return to df
auto_regression = function(temp_df,b0,b1)
{
  num_of_obs = length(temp_df[,"RSXFSN"])
  
  for(t in 2:num_of_obs)
    {
      temp_actual = temp_df[t,"RSXFSN"]
      temp_lag = temp_df[t,"lag_1"]
      
      temp_forecast = b0 + b1*temp_lag 
      
      temp_df[t,"forecast"] = temp_forecast
      temp_df[t,"residual"] = temp_actual-temp_forecast
      temp_df[t,"res_squared"] = (temp_actual-temp_forecast)^2
    }
  
  auto_regression = temp_df
}

updated_df = auto_regression(temp_df, b0, b1)
