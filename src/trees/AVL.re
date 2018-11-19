open BST_Streaming;

module MakeAVL = (Node: Comparable) : (S with type v := Node.t) => {
  include Make(Node);

  type node('v, 'f) =
    | Nil
    | Node(node('v, 'f), node('v, 'f), 'v, 'f);

  type tree('v) = node('v, unit);
};