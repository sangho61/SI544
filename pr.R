Recall the model from the homework that predicted the sales price of a home in Ann Arbor 48103 based on the number of square feet in the home.
The equation for the OLS regression line was: 
  saleprice = $49,531 + $149*squarefeet
There were 468 observations and the residual standard error was 103,900
The mean square feet for a house was 1,781 sq ft 
The total squared variation in X was 269,721,151
sum((sales_48103$squarefeet-mean(sales_48103$squarefeet,na.rm=TRUE))**2,na.rm=TRUE)
Suppose that we want a 90% confidence interval for the mean sales price for a 1,000 square foot home. 
The OLS regression line predicts  $49,531 + $149*1,000 = $198,531
The standard error is $103,900√(1/468+〖(1,000−1,781)〗^2/269,721,151)=$6,891
The number of standard errors is qt(0.95,468-2) = 1.65
The 90% confidence interval is:
  ($198,531 – 1.65*$6,891, 198,531 + 1.65*6,891) = ($187,160, $209,901)
R command: predict(model, newdata, interval = “confidence”, level=0.90)


Let’s calculate a 95% prediction interval for the sales price of a 1,000 square foot house.

The standard error is: 
  $103,900√(1+1/468+〖(1,000−1,781)〗^2/269,721,151)=$104,128

qt(0.975,466) = 1.97

The prediction interval is:
  ($198,531 – 1.97*$104,128, $198,531 + 1.97*$104,128) = (-$6,601, $403,663)
Note the prediction interval is poor even though R2 = 0.54.

R command: predict(model, newdata, interval=“prediction”, level=0.95)






Suppose that you want to buy a 1,000 square foot house.

The maximum amount that you can afford to pay is $195,000.

Can you estimate the percentage of houses in your price range?
  
  ($195,000 - $198,531) /$104,128 = -0.033

pt(-0.033, 466) = 48.7%




