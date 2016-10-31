module Tree

data Tree elem = Empty
               | Node (Tree elem) elem (Tree elem)
%name Tree tree, tree1, tree2

data BSTree : Type -> Type where
     BSEmpty : Ord elem => BSTree elem
     BSNode : Ord elem => (left : BSTree elem) -> (val: elem) -> (right : BSTree elem) -> BSTree elem

insert : elem -> BSTree elem -> BSTree elem
insert x BSEmpty = BSNode BSEmpty x BSEmpty
insert x orig@(BSNode left val right) = case compare x val of
                                             LT => BSNode (insert x left) val right
                                             EQ => orig
                                             GT => BSNode left val (insert x right)

listToTree : Ord a => List a -> BSTree a
listToTree [] = BSEmpty
listToTree (x :: xs) = insert x (listToTree xs)

treeToList : BSTree a -> List a
treeToList BSEmpty = []
treeToList (BSNode left val right) = (treeToList left) ++ (val :: treeToList right)
