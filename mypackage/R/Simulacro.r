#1 A company wants to evaluate customer satisfaction with its new product. A random sample of 200 customers was selected from Store A, and a random sample of 150 customers 
#was selected from Store B.In Store A, 120 out of 200 customers expressed satisfaction with the product. In Store B, 90 out of 150 customers expressed 
#satisfaction with the product. At a 5% significance level, calculate the confidence intervals for the proportion of satisfied customers in the store A.

n <- 200
x <- 120
pA <- x/n
alpha <- 0.05
z_alpha <- qnorm(1-alpha/2)
z_alpha
std_error <- sqrt((pA*(1-pA))/n)
lower_bound <- pA - z_alpha * std_error
upper_bound <- pA + z_alpha * std_error
lower_bound #0.5321049
upper_bound #0.6678951

#2
#A supermarket chain tracks its quarterly profits (in thousands of euros) between 2018 and 2022. The data is shown in the following table:
#OBTAIN Seasonal (E) * Random Component (A) = X/TC in

profits <- c(320.5, 345.7, 362.4, 390.8,
             335.2, 360.9, 375.6, 405.3,
             350.1, 378.3, 390.5, 420.7,
             365.4, 395.8, 405.7, 435.2,
             380.6, 410.4, 420.9, 450.0)

stats::filter(profits, c(0.5,1,1,1,0.5)/4)
uk <- ts(profits, start = c(2018, 1), frequency = 4)
uk
tend <- stats::filter(uk, c(0.5,1,1,1,0.5)/4)
tend
est_aleM <- uk/tend
est_aleM # 2020 Q3 1.0095330

#3
#The number of toys sold is:
#Year: 2000 2001 2002 2003
#Quantity: 3002 3700 3554 3272
#If we take the year 2000 as the base period, calculate all possible elementary indices.

P2000 <- 3002/3002
P2000
P2000 <- 3700/3002 
P2000
P2000 <- 3554/3002 
P2000
P2000 <- 3272/3002 
P2000

#4
#Get the random component of December 2002 from the following time series of miles travelled per month from 1992 to 2018:
data <- c(3459, 3458, 4002, 4564, 4221, 4529, 4466, 4137, 4126, 4259, 4240, 4936, 3031, 3261, 4160, 4377, 4307, 4696, 4458, 4457, 4364, 4236, 4500, 4974, 3075, 3377, 4443, 4261, 4460, 4985, 4324, 4719, 4374, 4248, 4784, 4971, 3370, 3484, 4269, 3994, 4715, 4974, 4223, 5000, 4235, 4554, 4851, 4826, 3699, 3983, 4262, 4619, 5219, 4836, 4941, 5062, 4365, 5012, 4850, 5097, 3758, 3825, 4454, 4635, 5210, 5057, 5231, 5034, 4970, 5342, 4831, 5965, 3796, 4019, 4898, 5090, 5237, 5447, 5435, 5107, 5515, 5583, 5346, 6286, 4032, 4435, 5479, 5483, 5587, 6176, 5621, 5889, 5828, 5849, 6180, 6771, 4243, 4952, 6008, 5353, 6435, 6673, 5636, 6630, 5887, 6322, 6520, 6678, 5082, 5216, 5893, 5894, 6799, 6667, 6374, 6840, 5575, 6545, 6789, 7180, 5117, 5442, 6337, 6525, 7216, 6761, 6958, 7070, 6148, 6924, 6716, 7975, 5326, 5609, 6414, 6741, 7144, 7133, 7568, 7266, 6634, 7626, 6843, 8540, 5629, 5898, 7045, 7094, 7333, 7918, 7289, 7396, 7259, 7268, 7731, 9058, 5557, 6237, 7723, 7262, 8241, 8757, 7352, 8496, 7741, 7710, 8247, 8902, 6066, 6590, 7923, 7335, 8843, 9327, 7792, 9156, 8037, 8640, 9128, 9545, 6627, 6743, 8195, 7828, 9570, 9484, 8608, 9543, 8123, 9649, 9390, 10065, 7093, 7483, 8365, 8895, 9794, 9977, 9553, 9375, 9225, 9948, 8758, 10839, 7266, 7578, 8688, 9162, 9369, 10167, 9507, 8923, 9272, 9075, 8949, 10843, 6558, 7481, 9475, 9424, 9351, 10552, 9077, 9273, 9420, 9413, 9866, 11455, 6901, 8014, 9832, 9281, 9967, 11344, 9106, 10469, 10085, 9612, 10328, 11483, 7486, 8641, 9709, 9423, 11342, 11274, 9845, 11163, 9532, 10754, 10953, 11922, 8395, 8888, 10110, 10493,
12218, 11385, 11186, 11462, 10494, 11540, 11138, 12709, 8557, 9059, 10055, 10977, 11792, 11904, 10965, 10981, 10828, 11817, 10470, 13310, 8400, 9062, 10722, 11107, 11508, 12904, 11869, 11224, 12022, 11983, 11506, 14183, 8648, 10321, 12107, 11420, 12238, 13681, 10950, 12700, 12272, 11905, 13016, 14421, 9043, 10452, 12481, 11491, 13545, 14730, 11416, 13402, 11907, 12711, 13261, 14265, 9564, 10415, 12683, 11919, 14138, 14583, 12640, 14257, 12396, 13914, 14174, 15504)
uk <- ts(data, start= c(1992, 1), frequency = 12)
uk
tend <- stats::filter(uk, c(0.5,1,1,1,1,1,1,1,1,1,1,1,0.5)/12)
seasonalRatios <- colMeans(matrix(uk/tend, ncol = 12, byrow=TRUE), na.rm = TRUE)
seasonalRatiosNormalized <- seasonalRatios/mean(seasonalRatios)
deseasonalized <- uk/seasonalRatiosNormalized 
randomComponent <- deseasonalized/tend
randomComponent #2002 Dec 1.0292989

#5
#The following table shows the quarterly revenue (in millions of euros) of a tech company from 2016 to 2020.
#Calculate the T*C value for the second quarter of 2020
data <- c(320.5, 340.1, 370.3, 400.2,
        420.7, 290.8, 315.4, 340.5,
        370.1, 390.3, 345.2, 360.8,
        380.9, 405.6, 430.5, 310.7,
        335.6, 355.7, 380.8, 410.9)
uk <- ts(data, c(2016,1), frequency = 4)
uk
tend <- stats::filter(uk, c(0.5,1,1,1,0.5)/4)
tend #358.2250 2020 Q2
