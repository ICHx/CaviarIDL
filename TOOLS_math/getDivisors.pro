FUNCTION getDivisors, value, MAX=max
	
	r = SQRT(value)+1
	IF KEYWORD_SET(max) && max LT r THEN N=max ELSE N=r
	
	
	; Get divisors from 1 to SQRT(n)
	d1 = LIST()
	FOR i=1, N DO IF value MOD i EQ 0 THEN d1.add, i
	d1=d1.ToArray()
	
	IF KEYWORD_SET(max) && max LT r THEN d=d1 $
	ELSE BEGIN
		; Get divisors from SQRT(value) to n witch are n/d1
		d2 = value/d1
		
		; Sort d2 in increasing order
		d2=d2[SORT(d2)]
	
		; Concatenate d1 and d2 divisors
		l1 = N_ELEMENTS(d1)
		IF d1[l1-1] EQ d2[0] THEN d=[d1[0:l1-2], d2] ELSE d=[d1, d2]
	END
	
	RETURN, d
END
