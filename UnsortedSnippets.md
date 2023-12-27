# Random Snippets & Infos
###### tags: `OCaml` `programming` 

## A
### Power function
> Not tail recursive
```ocaml
let rec power x n = if n<1 then 1 else x*power x (n-1)
```
### Modulo
```ocaml
let rec modulo x y = if x<y then x else modulo (x-y) y
```
### greatest common divisor (GCD)
> Example recursive function calls
```
gcd 21 15
gcd 15 6
gcd 6 3
gcd 3 0
3
```
### First
> Returns the first `int` from a starting point `k` which satisfies function `f` (predicate) of type `int -> bool`
```ocaml
let rec first f k = if (f k) then k else first f (k+1)
```
#### Function Type
``(int -> bool) -> int -> int``

#### Squareroot implementation using `first`
```ocaml
let sqrt n = first (fun k -> n < power(k+1) 2) 0
```

## B
### Invert fixed length tuple
```ocaml
let f (x,y,z) = (z,y,x)
```
### Digit sum
```ocaml
let rec dsum a x = if x<10 then a+x else dsum (a+x mod 10) (x/10)
```
### Inverse Function
> Find the inverse image of `x` under the function `f`.
```ocaml
let inv f x = first(fun k -> f (k+1) > x) 0
```
### Syntax
```ocaml
let add x y = x + y
(* add x y *)
(* add 1 1 *)
```
- x, y are the `parameters` of the function signature (variables)
- 1, 1 are the `arguments` passed to the function (concrete values)

```ocaml
if x < 100 then 10 else y
```
- Keywords: if, then, else
- Identifier: x, y
- Integer Literals: 100, 10
- Operator: <

### Grammar for Syntactic structure of types
```ocaml
〈type〉 ::= 〈base type〉
          | 〈type〉 → 〈type〉
          | 〈type〉1 × · · · × 〈type〉n (n≥2)
〈base type〉 ::= int | bool | string
```
## C
### 2
``let f = λx.λy.y in f``


## D
### Tuples
- return passed arguments as tuple
```ocaml
let test a b = (a,b)
```
- return tuple (a,b) with function f applied to b
```ocaml
let test f (a,b) = (a,f b)
```
### Iter function
```ocaml
let rec iter f n x = if n<1 then x else iter f (n-1) (f x)
```
Type

(α → α) → int → α → α

### Find the next prime number
```ocaml
let next_prime x = first isPrime (x+1)
let nthPrime n = iter next_prime n 2
```

## E
### Count elements in list that satisfy `f`
```ocaml
let rec countF f l = match l with
    | [] -> 0
    | x::l -> if f x then 1+countF f l else countF f l
```

### Membership function using CountF
```ocaml
let mem x l = countF (fun x -> x=y) l > 0
```

### Reverse a list
```ocaml
(* not tail recursive *)
let rec rev l = match l with
    | [] -> []
    | x::l -> (rev l) @ [x]
    
(* tail recursive *)
let rec foldl f l b =
  match l with
  | [] -> b
  | a :: l -> foldl f l (f a b)

let cons x l = x :: l
let rev l = foldl cons l []
```

## F

### Count occurrences of `x` in list `l`
```ocaml
let count x l = fold (fun x y -> 1+y) l 0
```
### Return nth option
> Option because of Some/None usage
```ocaml
let rec nth_opt l n = match l with
    | [] -> None
    | x::l -> if n<1 then Some x else nth_opt l (n-1)
```

### Lexical Order
> Checks if two lists are ordered in a lexically sorted manner based on the provided comparison function `p``.
```ocaml
let rec lex p l1 l2 =
  match l1, l2 with
  | [], _ -> true
  | _::_, [] -> false
  | x1::l1, x2::l2 -> p x1 x2 && if p x2 x1 then lex p l1 l2 else true
```

## G

### Depth tree
> Generate a binary tree recursively based on the value of `n`
```ocaml
type tree =
  | A
  | B of tree * tree

let rec dtree n = if n<=0 then A else B(dtree (n-1), A)
```
### Convert a binary tree into a string representation (infix notation)
```ocaml
let rec tree t = match t with
  | B(t1,t2) -> ptree t1 ^ "B" ^ tree t2
  | _ -> ptree t
and ptree t = match t with
  | A -> "A"
  | t -> "(" ^ tree t ^ ")"
```

### 4
```ocaml
type var = string
type exp = Var of var
        | Fapp of exp * exp
        | Lam of var * exp
        
let rec mem x l = match l with [] -> false | y::l -> x=y || mem x l
let rec f l e = 
        match e with
        | Var x -> mem x l
        | Fapp(e1,e2) -> f l e1 && f l e2
        | Lam(x,e) -> f (x::l) e
```

## H
### 1
```ocaml
let verify c l = match l with
  | [] -> failwith "verify : no token"
  | c':: l -> if c'=c then l else failwith "verify : wrong token"

let rec tree l = let (t,l) = ptree l in tree' t l
and tree' t l =
  match l with
  | BT::l -> let (t2,l) = ptree l in
      tree' (B(t,t2)) l
  | _ -> (t,l)
and ptree l = 
  match l with
  | AT::l -> (A,l)
  | LP::l -> let (t,l) = tree l in (t, verify RP l)
  | _ -> failwith "parse"
  
let checkParse (result,l) = 
if l = [] then result 
else failwith "checkParse: to many tokens"

let wrapper l = checkParse (tree l)
```
### 2
```
tree     ::= btree tree'
tree'    ::= "C" btree tree' | []
btree    ::= ptree btree'
btree'   ::= "B" ptree btree' | []
ptree    ::= "A" | "(" tree ")"
```
### Postfix parsing
```ocaml
let rec postfix l acc = 
  match l,acc with
  | AT::l, _ -> postfix l (A::acc)
  | BT::l, t1::t2::acc -> postfix l (B(t1,t2)::acc)
  | [], [acc] -> acc
  | _ -> failwith "parse postfix"
```

## J

### Reverse list `l`
> tail recursive
```ocaml
let rev l = foldl (fun x ls -> x::ls) l []
```
### Big O notation
> from best to worst runtime

log n ≤ n ≤ n*log n ≤ n^2 ≤ n^3 ≤ 2^n ≤ n!

## K
### Mathematial def. of gcd
```
gcd x y :=        y         if x = 0
                  x         if y = 0
            gcd x (y-x)     if 0<x≤y
            gcd y (x-y)     if 0<y<x
```
### Prime factorization invariant
> Given that k is greater than or equal to 2 and less than or equal to x. 

> Any p greater or equal 2 which divideds x has to be greater or equal k
```j
pfac k x := 2≤k≤x ∧ ∀p≥2. p|x → p≥k
```

## L

### Binary search on Array
```ocaml
let bsearch a x =
  let rec loop l r =
    if l > r then None
    else let m = (l+r) / 2 in
      match comp x a.(m) with
      | LE -> loop l (m-1)
      | EQ -> Some m
      | GR -> loop (m+1) r
  in loop 0 (Array.length a - 1)
```
### Quicksort on Array
```ocaml
let qsort a =
  in let rec qsort' l r =
       if l >= r then ()
       else let m = partition a l r in
         qsort' l (m-1); qsort' (m+1) r
  in qsort' 0 (Array.length a - 1)
```
### Find max element in Array
```ocaml
let array_max a = 
    let r = Array.length a-1 in
    let rec loop k j =
        if k>=r then a.(j)
        else if a.(j) > a.(k+1) then loop (k+1) (k+1)
        else loop (k+1) j
    in loop 0 0
```

### Generator
```ocaml
let exp () = let c = ref(3,3) in
    fun () -> let (x,y) = !c in (c:= (x+y,y); x)

let gen = exp ()

(* call gen () to increment *)
```

