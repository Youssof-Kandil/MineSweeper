type Cell = (Int,Int)
data MyState = Null | S Cell [Cell] String MyState deriving (Show,Eq)


up:: MyState -> MyState
up (S (0,_) _ _ _) = Null
up (S (a,b) list string mystate) =  S (a-1,b) list "up" (S (a,b) list string mystate)

down:: MyState -> MyState
down (S (3,_) _ _ _) = Null
down (S (a,b) list string mystate) =  S (a+1,b) list "down" (S (a,b) list string mystate)

left:: MyState -> MyState
left (S (_,0) _ _ _) = Null
left (S (a,b) list string mystate) =  S (a,b-1) list "left" (S (a,b) list string mystate)

right:: MyState -> MyState
right (S (_,3) _ _ _) = Null
right (S (a,b) list string mystate) =  S (a,b+1) list "right" (S (a,b) list string mystate)

collect:: MyState -> MyState
collect (S (x,y) [] _ _) = Null
collect (S p l string mystate) = S p (removePt p l) "Collect" (S p l string mystate)
														  
removePt pt [] = []
removePt (a,b)(x:xs) = if (a,b)== x then xs
									else x:(removePt (a,b) xs)
									
nextMyStates::MyState->[MyState]
nextMyStates (S p l s mystate) =  remove Null ([up (S p l s mystate)] ++ [down (S p l s mystate)] ++ [left (S p l s mystate)] ++ [right (S p l s mystate)] ++ [collect (S p l s mystate)])

remove:: MyState -> [MyState] -> [MyState]
remove _ [] = []
remove m (x:xs) = if (m == x) then remove m xs else x :remove m xs


isGoal::MyState->Bool
isGoal (S p l string mystate) = l == []

search::[MyState]->MyState
search [] = Null
search [Null] = Null
search (x:xs) = if (isGoal x) then x else search (xs ++ (nextMyStates x))

constructSolution:: MyState ->[String]
constructSolution Null = []
constructSolution (S p l s mystate)  = if (s == "") then constructSolution mystate else  constructSolution mystate ++ [s]

solve :: Cell->[Cell]->[String]
solve  cell [] = []
solve cell l = constructSolution (search [S cell l "" Null])






help c | c == ’a’ = ’b’
	   | c == ’b’ = ’c’
		| c == ’c’ = ’a’
		| otherwise = c
rotabc s = map help s