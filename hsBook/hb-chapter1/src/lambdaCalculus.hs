{-

'lambda calculus' or 'typed lambda calculus' is the foundation of the Haskell programming language.

f(x) = x + 1      this function named f takes on argument named x
                  and adds one to it.

                  so if 2 is passed in for x
f(2) = 2 + 1      

-}

addOne x =
  x + 1

-- the argument (x) is a placeholder for a value

{-

the lambda symbol in haskell is written as a backslash as in:    \

the lambda function has no name - it is an anonymous or lambda function.

this is what is called the identity function:    \x.x
the dot separates the 'function args' from the 'function body'

so
(\x.xy)z
    |__| replace the bound var x with z


\z.zy

and dropping the head(arg) \z. reduces to
zy

Currying is the rule that each lambda can take only one argument at a time.

so
\xy.xy   is shorthand for

\x.(\y.xy)   this makes the currying extpl



-}
   





  
