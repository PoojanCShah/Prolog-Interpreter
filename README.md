# COL226 Assignment 8 : PROLOG Implementation
#### Poojan C Shah 2022CS11594

## How To Run This Project ? 

1. `make init` to generate the lexer-parser-interpreter
2. Write your code in `<file>.pl`
3. `make prolog file = <file>.pl`
4. Interactive Interpreter is ready - type you queries.
5. `quit.` to exit
6. `make clean` to clean the directory.

## TestCase 0
```
/*Facts*/ 
studies(charlie, csc135).
studies(olivia, csc135).
studies(jack, csc131).  
studies(arthur, csc134).


teaches(kirke, csc135).  
teaches(collins, csc131). 
teaches(collins, csc171). 
teaches(juniper, csc134).
 

/*Rules*/

professor(X, Y) :-
teaches(X, C), studies(Y, C).

/* X is a professor of Y if X teaches C and Y studies C. */
```

```
?- studies(charlie,X).
true
X = csc135
?- studies(charlie,X),studies(olivia,X).
true
X = csc135
?- professor(kirke,olivia).
true
?- professor(X,arthur).
true
X = juniper
```



## TestCase 1

```
factorial(0, 1).
factorial(N, Result) :-
    N > 0,
    N1 is N - 1,
    factorial(N1, Result1),
    Result is N * Result1.

```
```
?- factorial(6,X).
true
X = 720
```
```
?- factorial(X,Y).
true
X = 0
Y = 1
```
```
?- factorial(5,X),factorial(4,Y), X > Y.
true
X = 120
Y = 24
```

## TestCase 2

```
append([], L, L).
append([H|T], L, [H|R]) :- append(T, L, R).
rev([], []).
rev([H|T], R) :- rev(T, R1), append(R1, [H|[]], R).
```

```
?- rev(X, [1|[2|[3|[]]]]).
true
X = [3|[2|[1|[]]]]
```

```
X = [3|[2|[1|[]]]]
?- append([1|[2|[]]], [a|[b|[]]], S).
true
S = [1|[2|[a|[b|[]]]]]
```


## TestCase 3

```
/*
fibonacci numbers function
*/

fib(0, 0).
fib(1, 1).
fib(N, F) :-
    N > 1,
    N1 is N - 1,
    N2 is N - 2,
    fib(N1, F1),
    fib(N2, F2),
    F is F1 + F2.
    
```

```
?- fib(10,N).
true
N = 55
```
```
?- fib(10,34).
fail
```

## TestCase 4

```
len([],0).
len([_|T],N) :- len(T,N1), N is N1+1.
```


```
?- len([a|[1|[b|[2|[]]]]],Z).
true
Z = 4
```