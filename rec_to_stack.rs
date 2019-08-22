enum Node {
    Node(i32, Box<Node>, Box<Node>),
    Empty,
}

impl Node {
    fn new(val: i32, left: Node, right: Node) -> Node {
        Node::Node(val, Box::new(left), Box::new(right))
    }

    fn leaf(val: i32) -> Node {
        Node::new(val, Node::Empty, Node::Empty)
    }

    fn empty() -> Node {
        Node::Empty
    }
}

enum Cont<'a> {
    Init(&'a Node),
    Right(i32, &'a Node),
}

fn in_order(node: &Node) -> Vec<i32> {
    let mut list = vec![];
    let mut stack = vec![Cont::Init(node)];
    while let Some(frame) = stack.pop() {
        match frame {
            Cont::Init(Node::Empty) => {}
            Cont::Init(Node::Node(v, l, r)) => {
                stack.push(Cont::Right(*v, r));
                stack.push(Cont::Init(l));
            }
            Cont::Right(v, r) => {
                list.push(v);
                stack.push(Cont::Init(r));
            }
        }
    }
    list
}

fn main() {
    let tree = Node::new(1, Node::leaf(2), Node::new(3, Node::leaf(4), Node::empty()));
    let list = in_order(&tree);
    println!("{:?}", list);
}
