fun getmax (xs : int list)= 
	if null xs then 0 
	else 
		let 
			val tl_ans = getmax(tl xs) 
		in 
			if hd xs > tl_ans then hd xs else tl_ans 
		end



getmax [1,2,3]

tl_ans = getmax [2,3]  hd xs = 1 -> if 1 > 3 then 1 else 3
tl_ans = getmax [3]    hd xs = 2 -> if 2 > 3 then 2 else 3
tl_ans = getmax []     hd xs = 3 -> if 3 > 0 then 3 else 0
tl_ans = 0


getmax [~5,~4,~2]

tl_ans = getmax [~4,~2]  hd xs = ~5 -> if ~5 > 0 then ~5 else 0
tl_ans = getmax [~2]    hd xs = ~4 -> if ~4 > 0 then ~4 else 0
tl_ans = getmax []     hd xs = ~2 -> if ~2 > 0 then ~2 else 0
tl_ans = 0





