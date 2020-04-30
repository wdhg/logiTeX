# logiTeX

logiTeX is a simple program to convert a basic language for logical statements into beautiful LaTeX.

## Example

**NOTE: many of the features in this example are not implmented yet. This example only acts as a desired target for the language.**

`````
// this is a comment that won't show up in the final document

// this defines the title of the document
// if multiple of these exist in the document, the last one will be the title
~ My super awesome LaTeX homework

// this is plain text that will be rendered left aligned and will wrap normally
Hello this is my questions answer sheet

// this is a question number
// the text after isn't rendered but instead replaced with a 1. in the left margin
# Question 1

// this is a sub-question
// again, this will place a (a) or (b) or (c) in the left margin
## Part (a)

// this is a sub-sub-question
### Part (i)

// this is an equation
// equation lines start with the character '%' followed by a space
% forall n in \N [n + 1 = succ(n) -> n + 1 in \N]

// this is a multiline equation, useful for breaking up long equations
%%%
A & = 3 + 4 \\
  & \qquad + 6 \\
  & \qquad + 3
%%%

// this is a code snippet
// each line will be enumerated on the left
```
isEven :: Int -> Bool
isEven x
  = x `mod` 2
```

// this is a LaTeX line
// regular latex can be placed on these lines and will be placed directly into the document
@ \newline\newline

// this is a multiline LaTeX section
@@@
\begin{center}
  This text will be center aligned in the document.
\end{center}
@@@

`````

## Todo / Ideas

- [ ] Better / easier multiline equations
- [ ] Escape LaTeX in non LaTeX areas
- [ ] Live editor and preview
  - [ ] Vim plugin
  - [ ] Live character rendering
  - [ ] Add natural deduction rendering
    - [ ] Verification
