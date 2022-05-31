#|
Tree graph of (count-change 11):

                          11,5
                          /   \
                     11,4      -39,5
                     /  \        |
                 11,3    -14,4   0
                /   \      |
             11,2   1,3    0
            /   \    |
         11,1   6,2  1
          |     /  \
          1  6,1   1,2
              |    / \
              1  1,1  -4,2
                  |     |
                  1     0

Hence, the sum of those leaves which values are 1 is 1+1+1+1 = 4. This
is the same answer as (count-change 11) => 4.
|#