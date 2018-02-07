
{--
Tokens will be of the following type:
    - The "swap" command, which will always be forced to take only two arguments; no further operations can be done on a row once it is swapped

The state of the matrix is unchanged between each execution frame (duh); instead, a new matrix is created.
Errors will not be handled gracefully. The program should just crash instead.

Input should be formatted as follows:
    - Each "frame" is restricted to one line, with plans to allow for a backslash character to extend the line in the future
    - Each line contains a semicolon or comma-separated series of functions to be applied in that order

Valid tokens:
    - delimiter ::= "," | ";" | "(" | ")" | <newline>
    - value     ::= scalar | row
    - scalar    ::= <any numeric character, hopefully matched by regex>
    - row       ::= "r"[scalar]
    - function  ::= operator | swap
    - operator  ::= "+" | "-" | "*" | "/"
    - swap      ::= "swap"

Precedence:
    - ","" and ";": separate commands
    - "swap" (prefix): must always take exactly two arguments and appear as its own delimited element
    - "(" and ")": redirect the syntax tree as necessary
    - "*" and "/"
    - "+" and "-"

Processing:
    1. The lexer creates an array of strings split by lines (excepting conjoined lines)
    2. Each of these lines is split into another array around delimiter characters
    3. Each line of tokens is then checked for validity
    4. Each line of tokens is turned into a node on the syntax tree

Validation:
    - The number of every row token must be positive and no larger than the height of the matrix
    - Making sure operators have valid arguments (+ and - take two scalars, * and / take two values), could be validated in evaluation

Say you have the equation
    r1 - (3/2 * r3 + 1/4 * (r2 + r1)) * 3
Here's how that's turned into a syntax tree:
    1. Tokenize [r1, -, (, 3, /, 2, *, r3, +, 1, /, 4, *, (, r2, +, r1, ), )]
       In RPN, this is
        (- q
            r1
            (* 
                (+ 
                    (* 
                        (/ 3 2) 
                        r3)
                    (*
                        (+ r2 r1)
                        (/ 1 4)))
            3))
    2. Search for parens and evaluate from within
        ( 3 / 2 * r3 + 1 / 4 * ( r2 + r1 ) )
        Since ASTs go low precedence to high precedence, search for add and sub first from right to left
           sub
          /      \
        r1        add
              /         \
           mul      
          /   \
       div     r3
      /   \
     3     2 
--}