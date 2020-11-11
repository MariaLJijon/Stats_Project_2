# Stats_Project_2
Applied Statistical Methods - Project 2
Assassination Classroom

Not too long ago I received very sensitive information notifying me that Martians were trying to invade the Earth. One very smart Martian had managed to infiltrate the planet by kidnapping and then taking the form of Wayne Lee. Alien Wayne set base in Missoula, Montana, where he was preparing for Earth’s invasion. It was fortunate my sources managed to get GPS data from Wayne’s phone for two weeks. Using that information, I will try to plant two bombs in Alien Wayne’s path to save the world. After all, 2020 has been crazy enough. Using this information, I devised a Bomb Plan, as follows: 

1.	Observe Wayne’s moving behavior: I will examine Wayne’s daily routine for two weeks based on his GPS data.

2.	Other possible variables: I will analyze if a variable such as temperature has an effect on how fast Wayne moves.

3.	Narrow the options: I will identify patterns to narrow down where/when to plant the bombs in his path. To do this I will choose the segment of the path where I have better information and where Wayne seems to be more consistent. 

4.	Remove noise in the chosen segment: I will use a Kalman filter and smoother on the segment of the journey.

5.	Pick possible bombing sites:  I will identify points (latitude and longitude) where the smoothed paths seem be closest to each other. I will also use information from Google Maps to confirm that Wayne passes these points every day. 

6.	Estimate the time to those points: I will use an OLS model to estimate the time it takes Wayne to arrive to those points. The model will be a function of longitude and latitude. 

7.	Model validation: The model will be validated and updated using two days of data which were not used in the model.

8.	Bombing sites/time selection: With this information I will select when and where to plant the bombs. 

9.	Algorithm: I will write and algorithm that will predict when Wayne will pass through the selected location. 


