data Tree a = Node a (Tree a) (Tree a) | Leaf deriving Show

-- (Node 10 (Node 5 (Node 2 Leaf Leaf) Leaf) (Node 15 Leaf Leaf))

preorder :: Tree a -> [a]
preorder Leaf = []
preorder (Node x l r) = [x] ++ (inorder l) ++ (inorder r)

inorder :: Tree a -> [a]
inorder Leaf = []
inorder (Node x l r) = (inorder l) ++ [x] ++ (inorder r)

postorder :: Tree a -> [a]
postorder Leaf = []
postorder (Node x l r) = (inorder l) ++ (inorder r) ++ [x]
