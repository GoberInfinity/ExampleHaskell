extract:: Integer -> [a] -> (a,[a])
extract 0 (h:t) = (h,t)
extract j l = loop j l []
	where
	    loop 0 (h:t) accum = (h,accum ++ t)
	    loop j (h:t) accum = loop (j-1) t (h:accum)

shuffle:: [b] -> [Integer] -> [b]
shuffle [e] [] = [e]
shuffle elements (r:r_others) = let (b,rest) = extract r elements
				in b:(shuffle rest r_others)

--shuffle ['a','b','c','d','e'] [3,1,1,0]
