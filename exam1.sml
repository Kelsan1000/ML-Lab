open IntInf

(* Kelsan Dorjee *)

(* Problem 3: Largest Prime Factor *)
(* Question: What is the largest prime factor of the 
number 600851475143 ? *)
(* Answer = 6857 *)

(* It takes in a list and returns the last element in the list*)
fun lastList(lis) =
	if length lis = 1 then lis
	else lastList(tl lis)

(* A helper function made for bigPrime. It returns the prime 
factors of an integer (num) assuming d is 2 *)
fun checksPrime(num, d) =
	if num < d 
		then [] 
	else if num mod d = 0 
		then d::checksPrime(num div d, d)
	else
		checksPrime(num, (d + 1))

(* This is the function used to solve problem 3. It takes an integer
and returns the largest prime factor in it *)
fun bigPrime(num) = lastList(checksPrime(num, 2))

(* Problem 5: Smallest Multiple*)
(*Question: What is the smallest positive number that is evenly 
divisible by all of the numbers from 1 to 20? *)
(* Answer = 232792560 *)
(* This function takes a while for it to compute the answer *)

(* This is used to solve problem 5. It takes an integer, f, 
and checks to see which number is divisible
without remainders from 1 to f *)
fun smallMultiple(f) =
	let
		val i = 1
		val count = 1
		fun works(i,f,c) =
			if i = f andalso c mod f = 0
				then c
			else if c mod i = 0
				then works(i + 1,f,c)
			else works(1,f,c + 1)
	in 
		works(i,f,count)
	end

(* Problem 6: Sum Square Difference *)
(* Question: Find the difference between the sum of the squares of the 
first one hundred natural numbers and the square of the sum. *)
(* Answer = 25164150 *)

(* It is used to find the sum of all the square numbers from i to x. 
acc is the accumulator value that is used to
help store the current total after each iteration. *)
fun squareSum(i,x,acc) =
			if i = x + 1
				then acc
			else squareSum(i+1,x,acc + (i * i))

(* It is used to find the square of the sum of the numbers from i to x.
acc is the accumulator used to help store the current sum after
each iteration *)
fun sumSquare(i,x,acc) =
			if i = x + 1
				then acc*acc
			else sumSquare(i+1,x,acc + i)

(* This is used to solve problem 6. It takes an integer and returns
the difference between the sum of the squares of the first x natural 
numbers and the square of the sum *)
fun ssd(x) = sumSquare(1,x,0) - squareSum(1,x,0)

(* Problem 16: Power Digit Sum *)
(* Question: What is the sum of the digits of the number 2^1000 *)
(* Answer = 1366 *)

(* This is a function that evaluates a number to power of the 
integer e *)
fun pow(n,e) =
	let
		val acc = n
		fun power(n,e,acc) =
			if e = 0
				then 1
			else if e = 1
				then acc
			else power(n,e-1,acc*n)
	in
		power(n,e,acc)
	end

(* This is used to evaluate the sum of the numbers in an integer n 
assuming count is 0. acc is used to hold the current sum *)
fun numSum(n,count,acc) =
	if n = 0
		then acc
	else if n mod 10 = 0
		then numSum(n div 10,count,acc)
	else if n mod 10 = count 
		then numSum(n - count,0,acc + count)
	else numSum(n,count + 1,acc)

(* This is used to solve problem 16. It finds the sum of the digits
of a number after having it be evaluated to the power of e *)
fun pds(n,e) = numSum(pow(n,e),0,0)

(* Problem 20: Factorial Digit Sum *)
(* Question: Find the sum of the digits in the number 100 *)
(* Answer = 648 *)

(* This is used as a factorial function *)
fun fact(n,acc) =
	if n = 1 orelse n = 0
		then acc
	else fact(n - 1,acc * n)

(* This is used to solve problem 20. It takes an integer n and finds 
the sum of the digits after finding the factorial of n *)
fun fds(n) = numSum(fact(n,1),0,0)


