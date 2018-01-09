
import random 

random.seed(1)

#Basic functions

random.random()

random.randint(0, 10000)

random.choice(range(1, 100))

random.gauss(5, 2)

random.sample(range(10000000), 5)


#System is constantly running the RN generation
random.getstate()

#Example of statistical bootstrapping using resampling with replacement to 
#estimate a confidence interval for the mean of a sample of size five

from statistics import mean
from random import choices
data = 1, 2, 4, 4, 10 
means = sorted(mean(choices(data, k=5)) for i in range(20))
print(f'The sample mean of {mean(data):.1f} has a 90% confidence '
      f'interval from {means[1]:.1f} to {means[-2]:.1f}')

#Another usecase
from random import expovariate, gauss
from statistics import mean, median, stdev

average_arrival_interval = 5.6
average_service_time = 5.0
stdev_service_time = 0.5

num_waiting = 0
arrivals = []
starts = []
arrival = service_end = 0.0
for i in range(20000):
    if arrival <= service_end:
        num_waiting += 1
        arrival += expovariate(1.0 / average_arrival_interval)
        arrivals.append(arrival)
    else:
        num_waiting -= 1
        service_start = service_end if num_waiting else arrival
        service_time = gauss(average_service_time, stdev_service_time)
        service_end = service_start + service_time
        starts.append(service_start)

waits = [start - arrival for arrival, start in zip(arrivals, starts)]
print(f'Mean wait: {mean(waits):.1f}.  Stdev wait: {stdev(waits):.1f}.')
print(f'Median wait: {median(waits):.1f}.  Max wait: {max(waits):.1f}.')

#Plotting random numbers (path)
import matplotlib.pyplot as plt
import random
x = []
a,seed,c,m,n = 128,10,0,509,500
for i in range (1,n):
   new_seed=(a*seed+c)%m
   seed = new_seed
   x.append(new_seed)


a,seed,c,m,n = 269,10,0,2048,500
y= []
for i in range (1,n):
   new_seed=(a*seed+c)%m
   seed = new_seed
   y.append( new_seed)
plt.plot(x,y)
plt.show()

#Scatterplot
plt.scatter(x, y)
plt.show()

