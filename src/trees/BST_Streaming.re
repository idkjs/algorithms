/**
Learn more about thunks, delay, force and streams from
Purely Functional Data Structures by Chris Okasaki
https://www.cs.cmu.edu/~rwh/theses/okasaki.pdf
*/
type thunk('v) = unit => 'v;
let force: thunk('v) => 'v = thunk => thunk();
let delay: 'v => thunk('v) = (value, ()) => value;

type stream('v) =
  | Empty
  | Cons('v, thunk(stream('v)));

/**
Similar to Ordering in Haskell
*/
type ord =
  | LT
  | EQ
  | GT;

/**
Similar to Either in Haskell
*/
type either('a, 'b) =
  | Left('a)
  | Right('b);

module type Comparable = {type t; let compare: (t, t) => ord;};

module type S = {
  type v;
  type tree('v);

  let preorder: tree(v) => stream(v);
  let inorder: tree(v) => stream(v);
  let postorder: tree(v) => stream(v);
  let dfs: tree(v) => stream(v);
  let bfs: tree(v) => stream(v);
  let empty: unit => tree(v);
  let add: (tree(v), v) => tree(v);
  /**
  I don't know how to constraint the type variable 'a to be Comparable,
  without knowing its concrete type of at the time the module is created.
  My only consolation is that to operate on tree('a) you need to create a Module with Make.
  You can only do this if the 'a is a comparable type.
  */
  let fmap: (v => 'a, tree(v)) => tree('a);
};

module Make = (Node: Comparable) : (S with type v := Node.t) => {
  type node('v) =
    | Nil
    | Node(node('v), node('v), 'v);

  type tree('v) = node('v);

  let preorder = node => {
    let rec aux = (~frontier) =>
      switch (frontier) {
      | [] => Empty
      | [h, ...tail] =>
        switch (h) {
        | Nil => aux(~frontier=tail)
        | Node(l, r, v) => Cons(v, delay(aux(~frontier=[l, r, ...tail])))
        }
      };

    aux(~frontier=[node]);
  };

  let inorder = node => {
    let rec aux = (~frontier) =>
      switch (frontier) {
      | [] => Empty
      | [Right(v), ...tail] => Cons(v, delay(aux(~frontier=tail)))
      | [Left(Nil), ...tail] => aux(~frontier=tail)
      | [Left(Node(l, r, v)), ...tail] =>
        aux(~frontier=[Left(l), Right(v), Left(r), ...tail])
      };

    aux(~frontier=[Left(node)]);
  };

  let postorder = node => {
    let rec aux = (~frontier) =>
      switch (frontier) {
      | [] => Empty
      | [Right(v), ...tail] => Cons(v, delay(aux(~frontier=tail)))
      | [Left(Nil), ...tail] => aux(~frontier=tail)
      | [Left(Node(l, r, v)), ...tail] =>
        aux(~frontier=[Left(l), Left(r), Right(v), ...tail])
      };

    aux(~frontier=[Left(node)]);
  };

  let dfs = preorder;

  let bfs = node => {
    let rec aux = (~curr, ~next) =>
      switch (curr, next) {
      | ([], []) => Empty
      | ([], _) => aux(~curr=next, ~next=[])
      | ([Nil, ...tail], _) => aux(~curr=tail, ~next)
      | ([Node(left, right, value), ...tail], _) =>
        Cons(value, delay(aux(~curr=tail, ~next=[left, right, ...next])))
      };

    aux(~curr=[node], ~next=[]);
  };

  let empty = () => Nil;

  let add = (root, x) => {
    let rec aux =
      fun
      | Nil => Node(Nil, Nil, x)
      | Node(left, right, v) =>
        switch (Node.compare(x, v)) {
        | LT => Node(aux(left), right, v)
        | GT => Node(left, aux(right), v)
        | EQ => Node(left, right, v)
        };

    aux(root);
  };

  let fmap = (f, root) => {
    let rec aux =
      fun
      | Nil => Nil
      | Node(left, right, v) => Node(aux(left), aux(right), f(v));

    aux(root);
  };
};