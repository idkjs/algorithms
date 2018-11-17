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
  type node;

  let preorder: node => stream(v);
  let inorder: node => stream(v);
  let postorder: node => stream(v);
  let dfs: node => stream(v);
  let bfs: node => stream(v);
  let empty: unit => node;
  let add: (node, v) => node;
};

module Make = (Node: Comparable) : (S with type v := Node.t) => {
  type node =
    | Nil
    | Node(node, node, Node.t);

  let preorder: node => stream(Node.t) =
    node => {
      let rec aux: (~frontier: list(node)) => stream(Node.t) =
        (~frontier) =>
          switch (frontier) {
          | [] => Empty
          | [h, ...tail] =>
            switch (h) {
            | Nil => aux(~frontier=tail)
            | Node(l, r, v) =>
              Cons(v, delay(aux(~frontier=[l, r, ...tail])))
            }
          };

      aux(~frontier=[node]);
    };

  let inorder: node => stream(Node.t) =
    node => {
      let rec aux: (~frontier: list(either(node, Node.t))) => stream(Node.t) =
        (~frontier) =>
          switch (frontier) {
          | [] => Empty
          | [Right(v), ...tail] => Cons(v, delay(aux(~frontier=tail)))
          | [Left(Nil), ...tail] => aux(~frontier=tail)
          | [Left(Node(l, r, v)), ...tail] =>
            aux(~frontier=[Left(l), Right(v), Left(r), ...tail])
          };

      aux(~frontier=[Left(node)]);
    };

  let postorder: node => stream(Node.t) =
    node => {
      let rec aux: (~frontier: list(either(node, Node.t))) => stream(Node.t) =
        (~frontier) =>
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

  let bfs: node => stream(Node.t) =
    node => {
      let rec aux: (~curr: list(node), ~next: list(node)) => stream(Node.t) =
        (~curr, ~next) =>
          switch (curr, next) {
          | ([], []) => Empty
          | ([], _) => aux(~curr=next, ~next=[])
          | ([Nil, ...tail], _) => aux(~curr=tail, ~next)
          | ([Node(left, right, value), ...tail], _) =>
            Cons(
              value,
              delay(aux(~curr=tail, ~next=[left, right, ...next])),
            )
          };

      aux(~curr=[node], ~next=[]);
    };

  let empty = () => Nil;

  let add: (node, Node.t) => node =
    (root, x) => {
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
};