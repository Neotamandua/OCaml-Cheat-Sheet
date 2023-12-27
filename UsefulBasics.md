# OCaml programming
###### tags: `OCaml` `programming` 
## Basics:
### Basic Functions
```ocaml
let min x y = if x < y then x else y

let to16 x =
  let a = x * x in
  let b = a * a in
  let c = b * b in c * c

(* A *) 
let rec power x n = if n < 1 then 1 else x * power x (n-1) 
let rec modulo x y = if x-y < 0 then x else modulo (x-y) (y)


let cuberoot n = first (fun k -> n < power(k+1) 3) 0

let rec cuberoot' k n =
  let cube k = k * k * k in
  if n < cube (k + 1) then k
  else cuberoot' (k + 1) n

(* B *)
let rec dsum x y = if y/10 = 0 then x+y else dsum (x+y mod 10) (y/10)
let cube x = x*x*x
let cuberoot x = inv cube x

(* C *)
let fst (x,y) = x
let snd (x,y) = y
let swap (x,y) = (y,x)
```
### Exceptions:
```ocaml
let exc1 = failwith "failtext"
exception Empty
exception Start_with_Uppercase
let exc2 = raise Empty
let exc3 = invalid_arg "failtext"
```
### (Higher order) Polymorphic Functions
```ocaml
let rec first f k =
  if f k then k
  else first f (k + 1)
  
let inv f x = first(fun k -> f (k+1) > x) 0 (* B *)

let rec iter f n x = if n<1 then x else iter f (n-1) (f x) (* C *)

let rec fold f l b =
  match l with
  | [] -> b
  | a :: l -> f a (fold f l b)
  
let rec foldl f l b =
  match l with
  | [] -> b
  | a :: l -> foldl f l (f a b)
  
let rec forall m n f : bool =
  m > n || f m && forall (m + 1) n f

(* there is also exists, iterup, iterdn *)
```

### Math Functions
```ocaml
let rec gcd x y = if y < 1 then x else gcd y (x mod y)

let rec lcd x y = first (fun k -> x mod k = 0 && y mod k = 0) 2

(* A *) 
let sqrt x = first (fun k -> (k+1)*(k+1) > x) 0

(* C *)
let isPrime x =  x <> 1 && (first (fun k -> x mod k = 0) 2) = x
let prime x = (x > 1) && forall 2 (x - 1) (fun k -> x mod k > 0) 
let nextPrime x = first isPrime (x+1)
let nthPrime n = iter nextPrime n 2 

let rec fib n = if n < 2 then n else fib (n - 2) + fib (n - 1) (* fib n *)

(* dynamic Programming *)
let pred n = fst (iter (fun (x,y) -> (y, y + 1)) n (0,0))
let fib' n  = fst (iter (fun (a,b) -> (b, a + b)) n (0,1)) (* fib n *)
let fac n = fst (iter (fun (a,i) -> (a*i,i+1)) n (1,1)) (* n! *)
```

## Lists:

### Basic Functions
```ocaml
let addone l = fold (fun a b -> (a+1)::b) l []
let cons x l = x :: l
let flatten l = fold app l [] (* or fold (@) l [] *)
```

### Important Functions
```ocaml
(* Tail recursive rev *)
let rev l = foldl cons l []
(* tail-recursive append *)
let app l1 l2 = fold cons l1 l2

(* Lexical order *)
let rec lex p l1 l2 =
  match l1, l2 with
  | [], _ -> true
  | _::_, [] -> false
  | x1::l1, x2::l2 -> p x1 x2 && if p x2 x1 then lex p l1 l2 else true
  
(* Prime factorization *)
let rec pfac x = let k = first (fun k -> x mod k = 0) 2 in 
    if k = x then [x] else k::pfac(x/k) 
                                                                                    
let pfac x = let rec pfac' k x = 
               if (k*k) > x then [x] 
               else if x mod k = 0
               then k :: pfac' k (x/k) 
               else pfac' (k+1) x
  in pfac' 2 x
```

### Powerlists:
```ocaml
(* Sublist/powerlist, prefix, suffix *)
let rec map f l =
  match l with
  | [] -> []
  | x :: l -> f x :: map f l

let rec pow l =
  match l with
  | [] -> [[]]
  | x :: l -> pow l @ List.map (fun l -> x :: l) (pow l)    
                
let rec pow l = match l with
  | [] -> [[]]
  | x::lr -> (pow lr) @ map (fun l -> x::l) (pow lr)
  
let rec pow l = match l with 
  | [] -> [[]]
  | x::l -> let cache = pow l in (* reduce function call from twice to once *)
      cache @ List.map (cons x) (cache)
                
let rec gpow l k =
  if k < 1 then [[]]
  else
    match l with
    | [] -> []
    | x :: l -> gpow l k @ List.map (List.cons x) (gpow l (k-1))
                  
let rec is_sublist l1 l2 =
  match l1, l2 with
  | l1, [] -> l1 = []
  | [], y::l2 -> true
  | x::l1, y::l2 -> is_sublist (x::l1) l2 || 
                    (x = y) && is_sublist l1 l2
                      
let rec suffixes l = match l with
  | [] -> [[]]
  | x::lr -> (x::lr)::(suffixes lr)

let rec prefixes l = match l with
  | [] -> [[]]
  | x::lr -> []:: (map (cons x) (prefixes lr))
```

## Trees:
> Lacks infix, suffix, prefix implementation
```ocaml                  
let rec size t =
  match t with
  | A -> 1
  | B(t1,t2) -> 1 + size t1 + size t2

let rec breadth t =
  match t with
  | A -> 1
  | B(t1,t2) -> breadth t1 + breadth t2
  
let rec depth t =
  match t with
  | A -> 0
  | B(t1,t2) -> 1 + max (depth t1) (depth t2)
  
let rec mirror t =
  match t with
  | A -> A
  | B(t1,t2) -> B(mirror t2, mirror t1)

(*infix,prefix,suffix*)
let rec btree t = match t with
  | B(t1,t2) -> ptree t1 ^ "B" ^ btree t2
  | t -> ptree t
and ptree t = match t with
  | A -> "A"
  | t -> "(" ^ btree t ^ ")"
  
type ctree = A | B of ctree * ctree | C of ctree * ctree

let rec ctree t = match t with
  | C(t1,t2) -> ctree t1 ^ "C" ^ btree t2
  | t -> btree t
and btree t = match t with
  | B(t1,t2) -> btree t1 ^ "B" ^ ptree t2
  | t -> ptree t
and ptree t = match t with
  | A -> "A"
  | t -> "(" ^ ctree t ^ ")" 
         
let t = C(C(A,A), C(B(A,A), A))
let t2 = C(A,A)
let test = ctree t
let test2 = ctree t2
```

## Non-Code:

There are multiple rules for evaluating and executing code:
- Evaluation-Rules
- Typing-Rules
- Binding-Rules

All of them are so-called inference rules.

### Right-Associative Grammar for syntactic structure of types:
```ocaml
〈type〉 ::= 〈base type〉
          | 〈type〉 → 〈type〉
          | 〈type〉1 × · · · × 〈type〉n (n≥2)
〈base type〉 ::= int | bool | string
```

### Abstract Syntax/Expressions of OCaml
```ocaml
types:
t ::= int | bool | string | t1 → t2 | t1 × · · · × tn (n≥2)
```

**abstract expressions** as described by the following **grammar**
```ocaml
e ::= c | x | e1 o e2 | e1 e2
    | IF e1 THEN e2 ELSE e3
    | (e1, . . . , en)         n≥2
    | πn e                     n≥1
    | LET x = e1 IN e2
    | λx.e
    | LET REC f x = e1 IN e2
```

#### Special Cases
let (𝑥, 𝑦) = 𝑧 in 𝑥 + 𝑦 (use graded projections 𝜋𝑛𝑖, i.e., think of 𝑧 as a tuple)

Solution:
$\pi_1^2z+\pi_2^2z$

or:

LET $z'$ = $z$ IN $\pi_1^2z'+ \pi_2^2z'$

---
let x = 5

Solution: LET x = 5 IN x

---
**Special case for (Polymorphic) Types [(α → β) →  γ]**
```ocaml
(* (rel. to halting problem/undecidable problem): *)
(α → β)
let rec f x = f x
```

### Simple Syntax Trees
```
1+2:
        +
        │
        │
        │
        │
┌───────┴───────┐
│               │
│               │
1               2

let x = 4

  let
  │ │
  │ │
  │ │
┌─┘ └─┐
│     │
x     4
```

### Inductive Proofs
> Mathematical induction

> IH = Induction Hypothesis

| Name               | Arguments                   | IH                     | Base Case                   | Induction Case           |
| ------------------ | --------------------------- | ---------------------- | --------------------------- | ------------------------ |
| List induction     | All Lists                   | if l = x::l' then l'   | []                          | x::l                     |
| Natural Induction  | All natural numbers         | if n=n'+1 then n'      | n=0                         | n>0/n=n'+1               |
| Induction          | All n∈(ℕ)                   | Valid ∀m∈ℕ. m<n        | m<n                         | n                        |
| Function Induction | Prop. of terminating funct. | Forall recursive calls | def. equ. without recursion | def. equ. with recursion |

