type node('k, 'v) =
  | Empty
  | Node(node('k, 'v), node('k, 'v), 'k, 'v);

let rec preorder = (root, f) =>
  switch (root) {
  | Empty => ()
  | Node(left, right, _key, value) =>
    f(value);
    preorder(left, f);
    preorder(right, f);
    ();
  };

let rec inorder = (root, f) =>
  switch (root) {
  | Empty => ()
  | Node(left, right, _key, value) =>
    inorder(left, f);
    f(value);
    inorder(right, f);
    ();
  };

let rec postorder = (root, f) =>
  switch (root) {
  | Empty => ()
  | Node(left, right, _key, value) =>
    postorder(left, f);
    postorder(right, f);
    f(value);
    ();
  };

let dfs = preorder;

exception Not_found;

let bfs = (root, f) => {
  let children =
    fun
    | Empty => []
    | Node(Empty, Empty, _, _) => []
    | Node(left, Empty, _, _) => [left]
    | Node(Empty, right, _, _) => [right]
    | Node(left, right, _, _) => [left, right];

  let next = frontier =>
    List.map(n => children(n), frontier) |> List.flatten;

  let value =
    fun
    | Empty => raise(Not_found)
    | Node(_, _, _, value) => value;

  let f = n => n |> value |> f |> ignore;

  let rec aux = frontier => {
    List.iter(f, frontier);
    aux(next(frontier));
  };

  aux([root]);
};

let add = (root, (k, v)) => {
  let rec aux =
    fun
    | Empty => Node(Empty, Empty, k, v)
    | Node(left, right, key, value) =>
      key <= k ?
        Node(aux(left), right, key, value) :
        Node(left, aux(right), key, value);
  aux(root);
};