Prototype Analysis README

Accomplished:
	1. Know how to process all the raw data (minus parallelized implementation):
		- most time-consuming was the regex to parse around weird nested punctuation in the raw problem data (the solution was to only match commas not followed by an even number of quotation marks)
	2. Know who to exclude/treat separately:
		- the ~3% of users (est: 405,131) who are teachers
		- the ~8% of users (est: 1,080,350) who are parents
	3. Understand important aspects of the population and missing data:
		- there are no users under age 13
		- 13% of the sample is coached by someone
		- the user data are completely unordered, sadly
		- it's possible that the math pretest is bad at discriminating between users of low ability

Up next:
	0. Still a few pieces of data that I need to get from KA (follow up with them)
	1. Write map-reduce code to get user/time-level summaries of the problem and video logs 
		- think carefully about what format I want the data in so I don't have to compute over the 400GB multiple times
	2. Write MPI code to analyze the user/time-level data efficiently
		- especially important for the time-level data because it will definitely need to be parallelized (with 12M users, that data can be analyzed on one machine with moderate patience for computationally intensive tasks)
