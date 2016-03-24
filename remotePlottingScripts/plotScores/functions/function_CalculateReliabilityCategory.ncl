; This is a function to calculate reliability category, given slope and intercept data from an attributes diagram weighted regression line (+ confidence intervals). Following the method from Weisheimer & Palmer 2013

; Dave MacLeod University of Oxford, 14th November 2013

undef("CalcRelCat")
function CalcRelCat(EventPerc,RegMaxInt,MinUncInt,MaxUncInt)

; Min- & MaxUncInt are the minimum and maximum of the Int of the confidence interval bounds at the right boundary (i.e. forecast prob = 1).

begin

NoSkillEnd = 1-(1-EventPerc)/2

if (RegMaxInt.le.EventPerc) then
	Cat= 1.0
end if


if (RegMaxInt.gt.EventPerc.and.RegMaxInt.lt.NoSkillEnd) then
	if (MinUncInt.le.EventPerc) then
		Cat= 2.0
	else
		Cat= 3.0
	end if
end if

if (RegMaxInt.ge.NoSkillEnd) then
	if (MinUncInt.le.EventPerc) then
		Cat= 2.0
	end if

	if (MinUncInt.le.NoSkillEnd.and.MinUncInt.gt.EventPerc) then
		Cat= 3.0
	end if

	if (MinUncInt.gt.NoSkillEnd.and.MinUncInt.le.1) then
		if (MaxUncInt.lt.1) then
			Cat= 4.0
		else
			Cat= 5.0
		end if
	end if

	if (MinUncInt.gt.1) then
		Cat= 4.0
	end if

end if

return(Cat) 

end
