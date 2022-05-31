#|
Tree graph of (count-change 11):

                           11,5
                          /    \
                     11,4       -39,5
                    /    \         |
                11,3      -14,4    0
               /    \        |
           11,2      1,3     0
          /   \      /  \
      11,1    6,2  -9,3  1,2
       |     /  \    |   /  \
       1  6,1   1,2  0  1,1  -4,2
           |    / \      |     |
           1  1,1  -4,2  1     0
               |     |
               1     0

Hence, the sum of those leaves which values are 1 is 1+1+1+1 = 4. This
is the same answer as (count-change 11)
=>
 amount: 11,	 kinds-of-coins: 5,	
 amount: -39,	 kinds-of-coins: 5,	
 amount: 11,	 kinds-of-coins: 4,	
 amount: -14,	 kinds-of-coins: 4,	
 amount: 11,	 kinds-of-coins: 3,	
 amount: 1,	 kinds-of-coins: 3,	
 amount: -9,	 kinds-of-coins: 3,	
 amount: 1,	 kinds-of-coins: 2,	
 amount: -4,	 kinds-of-coins: 2,	
 amount: 1,	 kinds-of-coins: 1,	
 amount: 11,	 kinds-of-coins: 2,	
 amount: 6,	 kinds-of-coins: 2,	
 amount: 1,	 kinds-of-coins: 2,	
 amount: -4,	 kinds-of-coins: 2,	
 amount: 1,	 kinds-of-coins: 1,	
 amount: 6,	 kinds-of-coins: 1,	
 amount: 11,	 kinds-of-coins: 1,	
;Value: 4
|#