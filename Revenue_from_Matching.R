Revenue_from_Matching <- function(A,B) 
{

#
# This method implements the revenue rate of the matching strategy with specifications A and B.
# Throughout the calculations the time is measured in minutes.
#

Game_Cost 		<- 2 / 60
lambda 			<- 120 / 60
mu 				<- 1
Tol 			<- 1e-4
T_Min 			<- 60 * 10000
T_Max 			<- 60 * 100000
t_step 			<- 60 * 1000

###########################################################################
#Initial Calculations
###########################################################################

Players 			<- rep(0,10)
Ind 				<- 1:10
Uniqueness 			<- pmin(abs(Ind - 5), abs(Ind - 6))
Exercise_Surplus 	<- array(rep(NA,10*10), dim=c(10,10))
Exercise_Value		<- array(rep(NA,10*10), dim=c(10,10))

for(si in 1:10) 
{
	for(sj in 1:si)
	{
		Exercise_Surplus[si,sj]	<- A + B * (Uniqueness[si] + Uniqueness[sj]) - abs(si - sj)
		Exercise_Surplus[sj,si]	<- Exercise_Surplus[si,sj]
		Exercise_Value[si,sj]	<- Game_Cost *(11 - abs(si - sj))
		Exercise_Value[sj,si]	<- Exercise_Value[si,sj]
	}
}

###########################################################################
# SIMULATION 
###########################################################################
t 					<- 0
t_old 				<- 0
Revenue 			<- 0
Old_Revenue_Rate	<- Inf
Converged 			<- FALSE
counter 			<- 1
set.seed(0) #reset starting point

while(t <= T_Max && !Converged) 
{
	N_pl 			<- sum(Players)
	dt 				<- -log(runif(1)) / (lambda + N_pl * mu) #in between time
	t 				<- t + dt

	if(N_pl > 0 && runif(1) <= N_pl * mu / (lambda + N_pl * mu)) 
	{
		# A player drops out.
		si 				<- sample(Ind[Players > 0], 1)		
		Players[si]		<- 0
	} 
	else 
	{
		# A new player signs up.
		si 				<- sample(1:10,1)	# Generating the skillset of the new player at random.

		if(Players[si] > 0) 
		{	
			# !!! PERFECT MATCH !!!
			Revenue 		<- Revenue + Exercise_Value[si,si] #Get match value from cache
			Players[si] 	<- 0
		} 
		else if(N_pl == 0) 
		{
			# The new player i is the only player on the platform.
			Players[si] 	<- 1
		} 
		else 
		{
			Crowd 			<- Ind[Players > 0]
			Ex_Surpluses	<- Exercise_Surplus[si,Crowd] #Compare to crowd members
			Max_Ex_Surplus 	<- max(Ex_Surpluses)

			if(Max_Ex_Surplus >= 0)	
			{
				# !!! MATCHING !!!
				sj 				<- min(Crowd[Ex_Surpluses == Max_Ex_Surplus])#default to lowest
				Revenue 		<- Revenue + Exercise_Value[si,sj]
				Players[sj] 	<- 0
			} 
			else 
			{
				# The new player i joins the crowd in waiting.
				Players[si] 	<- 1
			}
		}
	}

	Revenue_Rate			<- Revenue / t

	if(t >= T_Min && t - t_old >= t_step) 
	{
		Rel_Deviation		<- abs(log(Revenue_Rate) - log(Old_Revenue_Rate))
		Converged 			<- (Rel_Deviation <= Tol) #check for convergence
		t_old 				<- t
		Old_Revenue_Rate	<- Revenue_Rate	
	}	

	if(t >= counter * t_step) 
	{
		cat(paste('\n      After ', t / 60, ' hours the revenue rate is ', Revenue_Rate, '.', sep=''))
		counter 		<- counter + 1
	}
}

list(
		Revenue_Rate = Revenue_Rate, Converged = Converged, Rel_Deviation = Rel_Deviation
	)

}