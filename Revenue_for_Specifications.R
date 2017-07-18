Revenue_for_Specifications <- function(A_Vals, B_Vals) 
{
#A_Vals <- 0:9
#B_Vals <- seq(0,3,0.125)

A_Vals 			<- sort(A_Vals)
B_Vals 			<- sort(B_Vals)
A_Vals 			<- A_Vals[A_Vals >= 0 & A_Vals <= 9]
B_Vals 			<- B_Vals[B_Vals >= 0 & B_Vals <= 9]


Ind 			<- 1:10
Uniqueness 		<- pmin(abs(Ind - 5), abs(Ind - 6))

AN 				<- length(A_Vals)
BN 				<- length(B_Vals)
B_Max 			<- rep(NA,AN)

###########################################################################
# Preliminary Calculations

for(a in 1:AN) 
{
	A 			<- A_Vals[a]
	RHS_Vals 	<- c()

	for(si in c(1:10)) 
	{
		for(sj in 1:5) 
		{
			if(Uniqueness[si] + Uniqueness[sj] != 0)  
			{
				RHS_Vals <- c(RHS_Vals, (abs(si - sj) - A) / (Uniqueness[si] + Uniqueness[sj]))
			}
		}
	}

	B_Max[a] 	<- max(RHS_Vals) 
}

###########################################################################
# POPULATING THE TABLE WITH DIFFERENT SPECIFICATIONS

Table 					<- array(rep(NA,(BN+1)*(AN+1)), dim=c(BN+1,AN+1))
Table[1,2:(AN+1)]		<- A_Vals
Table[2:(BN+1),1]		<- B_Vals

cat('\n\nSIMULATIONS:')
for(a in 1:AN) 
{
	A 					<- A_Vals[a]
	B_Vals_Mod 			<- B_Vals[B_Vals < B_Max[a]]
	BN_Mod 				<- length(B_Vals_Mod)

	if(BN_Mod < BN) 
	{
		B_Vals_Mod 		<- c(B_Vals_Mod, B_Vals[BN_Mod + 1])
	}

	for(b in 1:length(B_Vals_Mod)) 
	{
		B 				<- B_Vals_Mod[b]
		cat(paste('\n   Processing A = ', A, ' and B = ', B, '.', sep=''))

		output 			<- Revenue_from_Matching(A,B)
		Table[b+1,a+1] 	<- output$Revenue_Rate		

		if(!output$Converged) 
		{
			cat(paste(' The simulation failed to converge: relative deviation = ', output$Rel_Deviation, '.', sep=''))
		}
	}
}
cat('\n')

write.csv(Table, file='Table.csv', row.names=F)
}