fun intsFromTo(lo,hi) = 
	if lo<=hi then lo::intsFromTo(lo+1,hi)  
	else []

fun take(numlis,x) = 
	if x>length numlis then numlis
	else if x=0 then []
	else hd numlis :: take(tl numlis , x-1)

fun drop(numlis,x) =
	if x>length numlis then []
	else if x=0 then numlis
	else drop(tl numlis, x-1)

fun interleave(numlis1,numlis2) =
	if numlis1 = [] andalso numlis2 = [] then []
	else if length numlis1 = 1 andalso numlis2 = []
		then hd numlis1 :: []
	else hd numlis1 :: hd numlis2 :: interleave(tl numlis1, tl numlis2)

fun shuffle(deck) =
	if length deck mod 2 = 1
		then let
			val deck1 = take(deck,(length deck + 1) div 2)
			val deck2 = drop(deck,(length deck + 1) div 2)
		in
			interleave(deck1,deck2)
		end
	else
		let
		 	val deck1 = take(deck,length deck div 2)
			val deck2 = drop(deck,length deck div 2)
		in
			interleave(deck1,deck2)
		end

fun shuffleNumber(decksize) =
	let
	 	val numlis = intsFromTo(1,decksize)
	 	val numlis2 = shuffle(numlis)
	 	val counter = 1
		fun newFunction(a,b,c) =
			if a = b 
			then c
			else newFunction(a,shuffle(b),c + 1)
	in 
		newFunction(numlis,numlis2,counter)
	end

fun isInOrder(numbers) =
	if numbers = [] then true
	else if length numbers = 1 then true
	else if hd numbers < hd (tl numbers) then isInOrder(tl numbers)
	else false

(* map (fn x=> shuffleNumber(x)) [5,50,500,5000]*)
