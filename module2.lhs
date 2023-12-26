	Prelude

> newtype M x = M (M x -> x) 

-- w = \x -> x x
> w :: (M x -> x) -> x
> w f = f (M f)

-- y f = f ( y f ) = f (f ( y f )) = ...
> y :: (a -> a) -> a
> y f = w (\(M x) -> f (w x))

-- qimokao on stack overflow ty


	Trees
	
	For the purposes of this problem set, a binary tree containing values of
	type a is defined as being either
	
	• empty; or
	• a node containing a value of type a and (recursively) two binary
	trees, referred to as the “left” and “right” subtrees.
	
Define a recursive, polymorphic algebraic data type Tree
which corresponds to the above definition.

> data Tree a where
>   Empty :: Tree a
>   Node  :: a -> Tree a -> Tree a -> Tree a

Define a function which adds one to every Integer contained in a tree.

> incrementTree :: Tree Integer -> Tree Integer
> incrementTree  Empty       = Empty
> incrementTree (Node n l r) = (Node (n+1)) (incrementTree l) (incrementTree r)

Define a function which computes the size of a tree, or number of nodes.

> treeSize :: Tree a -> Integer
> treeSize        Empty = 0
> treeSize (Node n l r) = 1 + (treeSize l) + (treeSize r)

	A binary search tree (BST) is a binary tree of Integers in which
	the Integer value stored in each node is larger than all the Integer
	values in its left subtree, and smaller than all the values in its right
	subtree. (For the purposes of this problem set, assume that all the
	values in a binary search tree must be distinct.)

> bstInsert :: Integer -> Tree Integer -> Tree Integer
> bstInsert i Empty        = Node i Empty Empty
> bstInsert i (Node j l r) = if i<j then (Node j (bstInsert i l) r)  
>                       else if i>j then (Node j l (bstInsert i r))  
>                       else             (Node i l r) 

	Given an integer i and a valid BST, bstInsert should produce an-
	other valid BST which contains i. If the input BST already contains i,
	it should be returned unchanged.

> isBST :: Tree Integer -> Bool
> isBST  Empty                                     = True
> isBST (Node n  Empty            Empty)           = True 
> isBST (Node n (Node l lt rt)    Empty)           = (n>l)&&(isBST (Node l lt rt)) 
> isBST (Node n  Empty           (Node r lt rt))   = (n<r)&&(isBST (Node r lt rt))
> isBST (Node n (Node l llt lrt) (Node r rlt rrt)) = (n>l)&&(isBST (Node r llt lrt))
>                                                  &&(n<r)&&(isBST (Node r rlt rrt))

Exercise 6 (Level 2) Ensure that your isBST function runs in O(n) time

> isBST2 :: Tree Integer -> Bool
> isBST2 t = isBSTHelper t Nothing Nothing

> isBSTHelper :: Tree Integer -> Maybe Integer -> Maybe Integer -> Bool
> isBSTHelper  Empty        _  _ = True
> isBSTHelper (Node n l r) mn mx = case (mn, mx) of
>   (Nothing, Nothing) -> (isBSTHelper l Nothing n)&&(isBSTHelper r n Nothing)
>   (a, Nothing)       -> (a<n)&&(isBSTHelper l a n)&&(isBSTHelper r n Nothing)
>   (Nothing, z)       -> (z>n)&&(isBSTHelper l Nothing n)&&(isBSTHelper r n z)
>   (a, z)             -> (isBSTHelper l a n)&&(isBSTHelper r n z)


	PROOF TREES
	
	A proposition is a logical assertion.
	
> type Prop = String

	A rule is a preordained operation by which one can manipulate propositions.
	
	For example, we might assert that X & Y & Z imply R, or
	
		X     Y     Z
		-------------
	  		  R		
	
	which we will represent as (A ["X", "Y", "Z"] "R").
	
	
> data Rule where
>   A :: [Prop] -> Prop -> Rule
  
	A rule system is a list of rules.

> type System = [Rule]

> data Proof where
>   PNode :: Rule -> [Proof] -> Proof
 
    root node -^
    chains of reasoning -^ 

	A Proof is a Tree where each node contains a rule and
	a list of proofs of the rule's premises.  For example,
	the proof tree

         ---
          X
   ---   ---   ---
    X     Y     Z
    -------------
          R

	would be represented as

	  (PNode (A ["X", "Y", "Z"] "R")
	    [ PNode (A [] "X") []
	    , PNode (A ["X"] "Y") [PNode (A [] "X") []]
	    , PNode (A [] "Z") []
	    ]
	  ) :: Proof

	One example of an invalid proof would be

	  (PNode (A ["A","B"] "C") [PNode (A [] "Q")])
	  
    This is invalid for two reasons: 
	  (1) the rule says it needs A, but	the first proof in the list is 
	      actually a proof of Q.  
	  (2) the rule says it needs two premises, A and B, but there is 
	      only one proof in	the list; there should be two.
		  
		  
	Exercize 7: Write a proof-checking function.
	
  	  things that need to be true:
		1. conclusion=proposition
		2. all node conclusions match stated rule premises
		3. proof is in order
		
> checkProof :: Proof -> Prop -> Bool 
> checkProof (PNode (A [] q) []) [] = True 
> checkProof (PNode (A ps q) ((PNode A (c:cs)):ns)) prop = 







	Exercise 9: Give a Proof Tree for each of the following.

	  a. (P ~> (Q ~> R)) |- (Q ~> (P ~> R))
		 
	  (PNode (A ["P"] "Q.R")
	    [ PNode (A [] "X") []
	    , PNode (A ["X"] "Y") [PNode (A [] "X") []]
	    , PNode (A [] "Z") []
	    ]
	  )