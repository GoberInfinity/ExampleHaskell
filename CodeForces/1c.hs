
distance ( [x0,y0], [x1,y1] )  = sqrt ( ( x1 - x0 )^2 + ( y1 - y0 )^2 )
angle (a,b,c) = acos ( ( c^2 + b^2 - a^2 ) / 2 / b / c )
isInteger x = abs ( x - fromIntegral ( round x ) ) <= 1e-5

area [p0,p1,p2] = let
		[a,b,c] = map distance [(p0,p1),(p1,p2),(p2,p0)]
                [x,y,z] = map angle [(a,b,c),(b,c,a),(c,a,b)]
                radius = a / 2 / sin x
                ok a = all isInteger $ map (/a) [x,y,z]
		q = 2 * ( head $ filter ok $ map (pi/) [3..100] )
                in
                  radius^2 * sin q * pi / q

main = interact $ show . area . map (map read . words) . lines
