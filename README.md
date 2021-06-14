# parallelCompilers

the three vectors describing AST:

depthVec :: Acc (Vector Int)    
depthVec = A.use $ fromList (Z:.9) [0,1,2,3,4,4,4,2,3]    
      
nodeVec :: Acc (Vector Char)    
nodeVec = A.use $ fromList (Z:.9) "FEFEVPVAN"    
     
valuesVec :: Acc (Vector Char)    
valuesVec = A.use $ fromList (Z:.9) "f000w+w07" 

The output is the matrix at the end of Section 3.2 of Hsu's article.

stack build
stack build
stack exec parallelCompilers-exe
