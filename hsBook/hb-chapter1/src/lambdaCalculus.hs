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



///////////////////////////

Currying is the rule that each lambda can take only one argument at a time.

so
\xy.xy   is shorthand for

\x.(\y.xy)   this makes the currying explicit

       


///////////////////////////


-- 1) (\abc.cba)zz(\wv.w)

--    (\a.\b.\c.cba)(z)z(\w.\v.w)   --explicit currying
--      *         |__|                 drop \a. head *

--    (\b.\c.cbz)(z)(\w.\v.w)
--      *     |___|                    drop \b. head

--    (\c.czz)(\w.\v.w)
--      * |   |_______|                drop \c. head

--    (\w.\v.w)(z)z
--      *    |__|                      drop \w. head

--    (\v.z)(z)
--        |__|                         drop \v. head

--        z          evaluates to z



||||||||||||||||||||||||||||||||||||||

one more example

(𝜆𝑥𝑦𝑧.𝑥𝑧(𝑦𝑧))(𝜆𝑚𝑛.𝑚)(𝜆𝑝.𝑝)
(𝜆𝑥.𝜆𝑦.𝜆𝑧.𝑥𝑧(𝑦𝑧))(𝜆𝑚.𝜆𝑛.𝑚)(𝜆𝑝.𝑝)     makes  currying  explicit
  *       *      |____1___|           bind x to first argument 1 
(𝜆𝑦.𝜆𝑧(𝜆𝑚.𝜆𝑛.𝑚)𝑧(𝑦𝑧))(𝜆𝑝.𝑝)          drop head y.
  *               *   |_2__|          bind y to next argument 2
𝜆𝑧(𝜆𝑚.𝜆𝑛.𝑚)(𝑧)((𝜆𝑝.𝑝)𝑧)
 *           *                       no arguments to apply to z
    *        *                       dive deeper 
𝜆𝑧(𝜆𝑛.𝑧)((𝜆𝑝.𝑝)𝑧)                    binding 𝑚 to the argument z
𝜆𝑛.𝑧                                 unconditionally tosses the argument
                                     ((𝜆𝑝.𝑝)𝑧)
                                     and returns
𝑧




that is all!
-}
   





  
