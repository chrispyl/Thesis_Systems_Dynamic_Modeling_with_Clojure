(in-ns 'infixapp.core)

(def topological-sort-1
	["x'=x+1#0"
	 "y'=x+y#0"
	 "z=x+1"
	 "w=a+c+1"
	 "c=z+1"
	 "a=c+z+2+y"])

(def topological-sort-2
	["x'=x+1#0"
	 "a=b+c+z"
	 "b=x+1"
	 "f=a+d"
	 "d=z+a"
	 "z=x+1"
	 "o=f+2"
	 "c=x+1"])	 
	 
(def test-signs-commas
	["ix' = min(1, -ix) ** (-ix) * ix - 0.02 * min(ix , -min(-ix , -ix) ) * ix * y#10"
	 "y' = ( -ix*ix)*0.02 * max(ix, -1) * ix * y - sin(0) * 0.4 * y# 10"
	 "i=0.5"
	 "aw=max(1,1)"
	 "c=ix+y-b"
	 "b=-0.3"])

(def test-substitutions
	["ix' = min(1, -ix) ** (-ix) * ix - 0.02 * min(ix , -min(-ix , -ix) ) * ix * y#10"
	 "y' = ( -ix*ix)*0.02 * max(ix, -1) * ix * y - sin(0) * 0.4 * y# 10"
	 "amaximi'= mina*2+amaximi#0"
	 "subu' = amaximi+mina*2#0"
	 "lufu' = amina*2+amaximi#0"
	 "maxi' = maxi#0"
	 "amaksi' = maxa#0"
	 "i=0.5"
	 "a=4"
	 "amina=0.2"
	 "maxa=0.4"
	 "aw=max(1,1)"
	 "c=ix+y-b"
	 "b=-0.3"
	 "mina=4"])	 
	 
(def math-forspace
	["P1' = (N/(M1+N)) * (PMAX1/K1) * ln((HSC1+IIN)/(HSC1+IOUT1)) - L1*P1 - A11*AL1*P1*H1#1"
	 "P2' = (N/(M2+N)) * (PMAX2/K2) * ln((HSC2+IOUT1)/(HSC2+IOUT2)) - L2*P2 - A12*AL2*P2*H1 - A22*AL2*P2*H2#1"
	 "H1' = C1*(A11*AL1*P1 + A12*AL2*P2)*H1 - MOR1*H1#1"
	 "H2' = C2*A22*AL2*P2*H2 - MOR2*H2#1"
	 "IOUT1 = IIN * exp(-K1*AL1*P1)"
	 "IOUT2 = IOUT1 * exp(-K2*AL2*P2)"
	 "LISTAR1 = MOR1/(C1*A11) - ((A12/A11)*(MOR2/(C2*A22)))"
	 "LISTAR2 = MOR2/(C2*A22)"
	 "IIN = 400"
	 "AL1 = 0.4	"
	 "AL2 = 0.7	"
	 "M1 = 400	"
	 "M2 = 200	"
	 "PMAX1 = 0.07"	
	 "PMAX2 = 0.04	"
	 "HSC1 = 50	"
	 "HSC2 = 10"
	 "K1 = 0.01	"
	 "K2 = 0.03	"
	 "L1 = 0.01	"
	 "L2 = 0.005"	
	 "A11 = 0.002"	
	 "A12 = 0.001"	
	 "A22 = 0.002"	
	 "C1 = 0.01	"
	 "C2 = 0.01"
	 "MOR1 = 0.002	"
	 "MOR2 = 0.002	"
	 "N = 100"	])	 
	 
(def floortest
	["x'= floor( x * 0.5) + 4#10"
	 "z = floor(3.4)"])	 
	 
(def predator-pray-mult
	["x' = 0.1 * x - 0.02 * x * y#10"
	 "y' = 0.02 * x * y - 0.4 * y# 10"
	 "a' = 0.1 * a - 0.02 * a * b# 10"
	 "b' = 0.02 * a * b - 0.4 * b# 10"
	 "k' = 0.1 * k - 0.02 * k * m# 10"
	 "m' = 0.02 * k * m - 0.4 * m# 10"
	 "z' = 0.001 * o + 0.001 * b# 10"
	 "w' = 0.001 * m + 0.001 * z# 10"
	 "o' = 0.001 * y + 0.001 * w# 10"])	 
	 
;this is a system map	 
(def teamming-test {:x {:init-val 0 :params [:b :o] :func ""}
					:y {:init-val 0 :params [:x :y :d] :func ""}
					:z {:init-val 0 :params [:y :z :k] :func ""}
					:a {:init-val 0 :params [:c] :func ""}
					:b {:init-val 0 :params [:a :b] :func ""}
					:c {:init-val 0 :params [:a :b] :func ""}
					:k {:init-val 0 :params [:l] :func ""}
					:l {:init-val 0 :params [:e] :func ""}
					:e {:init-val 0 :params [:k] :func ""}
					:d {:init-val 0 :params [:f :w] :func ""}
					:w {:init-val 0 :params [:d] :func ""}
					:f {:init-val 0 :params [:w] :func ""}
					:g {:init-val 0 :params [:o] :func ""}
					:h {:init-val 0 :params [:g] :func ""}
					:o {:init-val 0 :params [:h] :func ""}})

(def expansion-test1 [
				"a'=a+1#0"
				"b'=a+2#0"
				"c'=b+2#0"
				
				"d'=d+1#0"
				"f'=d+2#0"
				"g'=f+2#0"
				
				"h'=c+g#0"
				"i'=h+1#0"
			])

(def expansion-test2 [
				"a'=a+1#0"
				"b'=a+v#0"
				"v'=b+2#0"
				"c'=v+2#0"
				
				"d'=d+1#0"
				"f'=d+2#0"
				"g'=f+2#0"
				
				"h'=c+g#0"
				"i'=h+1#0"
			])

(def expansion-test3 [
				"a'=a+1#0"
				"b'=a+1#0"
				"c'=b+a#0"
				
				"d'=b+h#0"
				"e'=c+d+i#0"
				
				"f'=g+1#0"
				"g'=f+1#0"
				"h'=f+1#0"
				"i'=g+f#0"
			])			
			
(def big-system [
					"a' = (a + u) * (d * j)#0"
					"b' = (y + i + b) * (y * i * b)#0"
					"c' = (q + m + p + s + h + c) * (q * m * p * s * h * c)#0"
					"d' = (o + a5) * (t * p * a5)#0"
					"f' = (m + g) * (s * f)#0"
					"g' = (m + g) * (m * g)#0"
					"h' = (h + h)* (h * h)#0"
					"i' =  y * y #0"
					"j' = (a + u) * g #0"
					"k' = 2 * k#0"
					"l' = 2 * k * l * n#0"
					"m' = 2 * m * z#0"
					"n' = 2 * l#0"
					"o' = 2 * o#0"
					"p' = 2 * f * a * z#0"
					"q' = 2 * b * x * y * z#0"
					"r' = 2 * a1 * l#0"
					"s' = 2 * r#0"
					"t' = 2 * o#0"
					"u' = 2 * a * f#0"
					"x' = 2 * x#0"
					"y' = 2 * k * l#0"
					"z' = (t + 2) * (g  * c)#0"
					"a1' = (a6 + l) * (x * x)#0"
					"a3' = (a3 + 9) * (9 * 0.1)#0"
					"a5' = (m + 1) * (m * 3)#0"
					"a6' = (m + 8) * (m * 6)#0"
					"a7' = (x + 0) * (z * x)#0"
				])					

(def huge-system [
					"a' = sqrt(a * b - c - d * f - g + h - (i + j + k))# 0"
					"b' = sqrt(a * b - c - d * f - g + h - (i + j + k))# 0"
					"c' = sqrt(a * b - c - d * f - g + h - (i + j + k))# 0"
					"d' = sqrt(a * b - c - d * f - g + h - (i + j + k))# 0"
					"f' = sqrt(a * b - c - d * f - g + h - (i + j + k))# 0"
					"g' = sqrt(a * b - c - d * f - g + h - (i + j + k))# 0"
					"h' = sqrt(a * b - c - d * f - g + h - (i + j + k))# 0"
					"i' = sqrt(a * b - c - d * f - g + h - (i + j + k))# 0"
					"j' = sqrt(a * b - c - d * f - g + h - (i + j + k))# 0"
					"k' = sqrt(a * b - c - d * f - g + h - (i + j + k))# 0"
					
					"a1' = sqrt(a1 * b1 - c1 - d1 * f1 - g1 + h1 - (i1 + j1 + k1))# 0"
					"b1' = sqrt(a1 * b1 - c1 - d1 * f1 - g1 + h1 - (i1 + j1 + k1))# 0"
					"c1' = sqrt(a1 * b1 - c1 - d1 * f1 - g1 + h1 - (i1 + j1 + k1))# 0"
					"d1' = sqrt(a1 * b1 - c1 - d1 * f1 - g1 + h1 - (i1 + j1 + k1))# 0"
					"f1' = sqrt(a1 * b1 - c1 - d1 * f1 - g1 + h1 - (i1 + j1 + k1))# 0"
					"g1' = sqrt(a1 * b1 - c1 - d1 * f1 - g1 + h1 - (i1 + j1 + k1))# 0"
					"h1' = sqrt(a1 * b1 - c1 - d1 * f1 - g1 + h1 - (i1 + j1 + k1))# 0"
					"i1' = sqrt(a1 * b1 - c1 - d1 * f1 - g1 + h1 - (i1 + j1 + k1))# 0"
					"j1' = sqrt(a1 * b1 - c1 - d1 * f1 - g1 + h1 - (i1 + j1 + k1))# 0"
					"k1' = sqrt(a1 * b1 - c1 - d1 * f1 - g1 + h1 - (i1 + j1 + k1))# 0"
					
					"a2' = sqrt(a2 * b2 - c2 - d2 * f2 - g2 + h2 - (i2 + j2 + k2))# 0"
					"b2' = sqrt(a2 * b2 - c2 - d2 * f2 - g2 + h2 - (i2 + j2 + k2))# 0"
					"c2' = sqrt(a2 * b2 - c2 - d2 * f2 - g2 + h2 - (i2 + j2 + k2))# 0"
					"d2' = sqrt(a2 * b2 - c2 - d2 * f2 - g2 + h2 - (i2 + j2 + k2))# 0"
					"f2' = sqrt(a2 * b2 - c2 - d2 * f2 - g2 + h2 - (i2 + j2 + k2))# 0"
					"g2' = sqrt(a2 * b2 - c2 - d2 * f2 - g2 + h2 - (i2 + j2 + k2))# 0"
					"h2' = sqrt(a2 * b2 - c2 - d2 * f2 - g2 + h2 - (i2 + j2 + k2))# 0"
					"i2' = sqrt(a2 * b2 - c2 - d2 * f2 - g2 + h2 - (i2 + j2 + k2))# 0"
					"j2' = sqrt(a2 * b2 - c2 - d2 * f2 - g2 + h2 - (i2 + j2 + k2))# 0"
					"k2' = sqrt(a2 * b2 - c2 - d2 * f2 - g2 + h2 - (i2 + j2 + k2))# 0"
					
					"a3' = sqrt(a3 * b3 - c3 - d3 * f3 - g3 + h3 - (i3 + j3 + k3))# 0"
					"b3' = sqrt(a3 * b3 - c3 - d3 * f3 - g3 + h3 - (i3 + j3 + k3))# 0"
					"c3' = sqrt(a3 * b3 - c3 - d3 * f3 - g3 + h3 - (i3 + j3 + k3))# 0"
					"d3' = sqrt(a3 * b3 - c3 - d3 * f3 - g3 + h3 - (i3 + j3 + k3))# 0"
					"f3' = sqrt(a3 * b3 - c3 - d3 * f3 - g3 + h3 - (i3 + j3 + k3))# 0"
					"g3' = sqrt(a3 * b3 - c3 - d3 * f3 - g3 + h3 - (i3 + j3 + k3))# 0"
					"h3' = sqrt(a3 * b3 - c3 - d3 * f3 - g3 + h3 - (i3 + j3 + k3))# 0"
					"i3' = sqrt(a3 * b3 - c3 - d3 * f3 - g3 + h3 - (i3 + j3 + k3))# 0"
					"j3' = sqrt(a3 * b3 - c3 - d3 * f3 - g3 + h3 - (i3 + j3 + k3))# 0"
					"k3' = sqrt(a3 * b3 - c3 - d3 * f3 - g3 + h3 - (i3 + j3 + k3))# 0"
					
					"a4' = sqrt(a4 * b4 - c4 - d4 * f4 - g4 + h4 - (i4 + j4 + k4))# 0"
					"b4' = sqrt(a4 * b4 - c4 - d4 * f4 - g4 + h4 - (i4 + j4 + k4))# 0"
					"c4' = sqrt(a4 * b4 - c4 - d4 * f4 - g4 + h4 - (i4 + j4 + k4))# 0"
					"d4' = sqrt(a4 * b4 - c4 - d4 * f4 - g4 + h4 - (i4 + j4 + k4))# 0"
					"f4' = sqrt(a4 * b4 - c4 - d4 * f4 - g4 + h4 - (i4 + j4 + k4))# 0"
					"g4' = sqrt(a4 * b4 - c4 - d4 * f4 - g4 + h4 - (i4 + j4 + k4))# 0"
					"h4' = sqrt(a4 * b4 - c4 - d4 * f4 - g4 + h4 - (i4 + j4 + k4))# 0"
					"i4' = sqrt(a4 * b4 - c4 - d4 * f4 - g4 + h4 - (i4 + j4 + k4))# 0"
					"j4' = sqrt(a4 * b4 - c4 - d4 * f4 - g4 + h4 - (i4 + j4 + k4))# 0"
					"k4' = sqrt(a4 * b4 - c4 - d4 * f4 - g4 + h4 - (i4 + j4 + k4))# 0"
					
					"a5' = sqrt(a5 * b5 - c5 - d5 * f5 - g5 + h5 - (i5 + j5 + k5))# 0"
					"b5' = sqrt(a5 * b5 - c5 - d5 * f5 - g5 + h5 - (i5 + j5 + k5))# 0"
					"c5' = sqrt(a5 * b5 - c5 - d5 * f5 - g5 + h5 - (i5 + j5 + k5))# 0"
					"d5' = sqrt(a5 * b5 - c5 - d5 * f5 - g5 + h5 - (i5 + j5 + k5))# 0"
					"f5' = sqrt(a5 * b5 - c5 - d5 * f5 - g5 + h5 - (i5 + j5 + k5))# 0"
					"g5' = sqrt(a5 * b5 - c5 - d5 * f5 - g5 + h5 - (i5 + j5 + k5))# 0"
					"h5' = sqrt(a5 * b5 - c5 - d5 * f5 - g5 + h5 - (i5 + j5 + k5))# 0"
					"i5' = sqrt(a5 * b5 - c5 - d5 * f5 - g5 + h5 - (i5 + j5 + k5))# 0"
					"j5' = sqrt(a5 * b5 - c5 - d5 * f5 - g5 + h5 - (i5 + j5 + k5))# 0"
					"k5' = sqrt(a5 * b5 - c5 - d5 * f5 - g5 + h5 - (i5 + j5 + k5))# 0"
					
					"a6' = sqrt(a6 * b6 - c6 - d6 * f6 - g6 + h6 - (i6 + j6 + k6))# 0"
					"b6' = sqrt(a6 * b6 - c6 - d6 * f6 - g6 + h6 - (i6 + j6 + k6))# 0"
					"c6' = sqrt(a6 * b6 - c6 - d6 * f6 - g6 + h6 - (i6 + j6 + k6))# 0"
					"d6' = sqrt(a6 * b6 - c6 - d6 * f6 - g6 + h6 - (i6 + j6 + k6))# 0"
					"f6' = sqrt(a6 * b6 - c6 - d6 * f6 - g6 + h6 - (i6 + j6 + k6))# 0"
					"g6' = sqrt(a6 * b6 - c6 - d6 * f6 - g6 + h6 - (i6 + j6 + k6))# 0"
					"h6' = sqrt(a6 * b6 - c6 - d6 * f6 - g6 + h6 - (i6 + j6 + k6))# 0"
					"i6' = sqrt(a6 * b6 - c6 - d6 * f6 - g6 + h6 - (i6 + j6 + k6))# 0"
					"j6' = sqrt(a6 * b6 - c6 - d6 * f6 - g6 + h6 - (i6 + j6 + k6))# 0"
					"k6' = sqrt(a6 * b6 - c6 - d6 * f6 - g6 + h6 - (i6 + j6 + k6))# 0"
					
					"a7' = sqrt(a7 * b7 - c7 - d7 * f7 - g7 + h7 - (i7 + j7 + k7))# 0"
					"b7' = sqrt(a7 * b7 - c7 - d7 * f7 - g7 + h7 - (i7 + j7 + k7))# 0"
					"c7' = sqrt(a7 * b7 - c7 - d7 * f7 - g7 + h7 - (i7 + j7 + k7))# 0"
					"d7' = sqrt(a7 * b7 - c7 - d7 * f7 - g7 + h7 - (i7 + j7 + k7))# 0"
					"f7' = sqrt(a7 * b7 - c7 - d7 * f7 - g7 + h7 - (i7 + j7 + k7))# 0"
					"g7' = sqrt(a7 * b7 - c7 - d7 * f7 - g7 + h7 - (i7 + j7 + k7))# 0"
					"h7' = sqrt(a7 * b7 - c7 - d7 * f7 - g7 + h7 - (i7 + j7 + k7))# 0"
					"i7' = sqrt(a7 * b7 - c7 - d7 * f7 - g7 + h7 - (i7 + j7 + k7))# 0"
					"j7' = sqrt(a7 * b7 - c7 - d7 * f7 - g7 + h7 - (i7 + j7 + k7))# 0"
					"k7' = sqrt(a7 * b7 - c7 - d7 * f7 - g7 + h7 - (i7 + j7 + k7))# 0"
					
					"a8' = sqrt(a8 * b8 - c8 - d8 * f8 - g8 + h8 - (i8 + j8 + k8))# 0"
					"b8' = sqrt(a8 * b8 - c8 - d8 * f8 - g8 + h8 - (i8 + j8 + k8))# 0"
					"c8' = sqrt(a8 * b8 - c8 - d8 * f8 - g8 + h8 - (i8 + j8 + k8))# 0"
					"d8' = sqrt(a8 * b8 - c8 - d8 * f8 - g8 + h8 - (i8 + j8 + k8))# 0"
					"f8' = sqrt(a8 * b8 - c8 - d8 * f8 - g8 + h8 - (i8 + j8 + k8))# 0"
					"g8' = sqrt(a8 * b8 - c8 - d8 * f8 - g8 + h8 - (i8 + j8 + k8))# 0"
					"h8' = sqrt(a8 * b8 - c8 - d8 * f8 - g8 + h8 - (i8 + j8 + k8))# 0"
					"i8' = sqrt(a8 * b8 - c8 - d8 * f8 - g8 + h8 - (i8 + j8 + k8))# 0"
					"j8' = sqrt(a8 * b8 - c8 - d8 * f8 - g8 + h8 - (i8 + j8 + k8))# 0"
					"k8' = sqrt(a8 * b8 - c8 - d8 * f8 - g8 + h8 - (i8 + j8 + k8))# 0"
					
					"a9' = sqrt(a9 * b9 - c9 - d9 * f9 - g9 + h9 - (i9 + j9 + k9))# 0"
					"b9' = sqrt(a9 * b9 - c9 - d9 * f9 - g9 + h9 - (i9 + j9 + k9))# 0"
					"c9' = sqrt(a9 * b9 - c9 - d9 * f9 - g9 + h9 - (i9 + j9 + k9))# 0"
					"d9' = sqrt(a9 * b9 - c9 - d9 * f9 - g9 + h9 - (i9 + j9 + k9))# 0"
					"f9' = sqrt(a9 * b9 - c9 - d9 * f9 - g9 + h9 - (i9 + j9 + k9))# 0"
					"g9' = sqrt(a9 * b9 - c9 - d9 * f9 - g9 + h9 - (i9 + j9 + k9))# 0"
					"h9' = sqrt(a9 * b9 - c9 - d9 * f9 - g9 + h9 - (i9 + j9 + k9))# 0"
					"i9' = sqrt(a9 * b9 - c9 - d9 * f9 - g9 + h9 - (i9 + j9 + k9))# 0"
					"j9' = sqrt(a9 * b9 - c9 - d9 * f9 - g9 + h9 - (i9 + j9 + k9))# 0"
					"k9' = sqrt(a9 * b9 - c9 - d9 * f9 - g9 + h9 - (i9 + j9 + k9))# 0"
					
					"aa' = sqrt(a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9) + 2 * (aa ** vv)# 0"
					"bb' =  2 * (bb ** aa)# 0"
					"cc' =  2 * (cc ** bb)# 0"
					"dd' =  2 * (dd ** cc)# 0"
					"ff' =  2 * (ff ** dd)# 0"
					"gg' =  2 * (gg ** ff)# 0"
					"hh' =  2 * (hh ** gg)# 0"
					"ii' =  2 * (ii ** hh)# 0"
					"jj' =  2 * (jj ** ii)# 0"
					"kk' =  2 * (kk ** jj)# 0"
					"ll' =  2 * (ll ** kk)# 0"
					"mm' =  2 * (mm ** ll)# 0"
					"nn' =  2 * (nn ** mm)# 0"
					"oo' =  2 * (oo ** nn)# 0"
					"pp' =  2 * (pp ** oo)# 0"
					"rr' =  2 * (rr ** pp)# 0"
					"ss' =  2 * (ss ** rr)# 0"
					"tt' =  2 * (tt ** ss)# 0"
					"uu' =  2 * (uu ** tt)# 0"
					"vv' =  2 * (vv ** uu)# 0"
				])				
				
					