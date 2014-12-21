> module Custom.Data.BinaryTree
>     ( Tree
>     , empty
>     , insert
>     , find
>     )
> where

> import qualified Data.Foldable as Fold

We want to describe what a tree is.  It tree consists of nodes that
are either leafs or branches.  A leaf is a node with no children and
without data.  A branch is a node that has data, a left child and a
right child.  The left child stores all values that are smaller or
equal than the value stored in the node and the right child stores
only values that are bigger than the value stored in the node.

> data Tree a = Leaf | Branch a (Tree a) (Tree a)
>               deriving (Show)

An empty tree is only a leaf.
                
> empty :: Tree a
> empty = Leaf

We can insert a value into an existing tree.  We want to keep our tree
ordered and thats why we need our values to of a type that is an
instance of Ord.

> insert :: (Ord a) => a -> Tree a -> Tree a

If we find ourselves looking at an empty tree (leaf) than we replace
it by a branch storing the value to be inserted with two empty
children.

> insert x Leaf = Branch x Leaf Leaf

When inserting a value (y) we have to look at the node at the top of
the tree.  When the value stored in then node (y) is smaller than x
then we know that have to add x to the right tree and otherwise we
have to add the value to the left tree.

> insert x (Branch y left right)
>     | x > y = Branch y left (insert x right)
>     | otherwise = Branch y (insert x left) right

When we need to know if a value is stored in the tree then we have to
look at the top of the tree.  If the value y of the node is equal to
the value we are looking for then we are done and return just the
value.  If y is smaller than x then we look at the right child of the
node and otherwise we look at the left child of node.  If we look at
an empty tree then we return Nothing because we no that the right
value is not in the tree.

> find :: (Ord a) => Tree a -> a -> Maybe a
> find Leaf _ = Nothing
> find (Branch x left right) y
>     | x == y = Just x
>     | x < y = find right y
>     | otherwise = find left y

We want to define what it means that our tree is foldable.

> instance Fold.Foldable Tree where

A fold of Leaf is just the starting value.   

>     foldr _ ac Leaf = ac
>     foldl _ ac Leaf = ac

The right fold of a Branch with a function 'f' is the fold of the left
subtree and the value itself used as arguments for 'f'.  The resulting
value is the starting value for the fold of the right subtree.  The
left fold of a branch is a right fold with the left and the right
subtree exchanged.

>     foldr f ac (Branch val left right) =
>         Fold.foldr f (f val (Fold.foldr f ac right)) left
>     foldl f ac (Branch val left right) =
>         Fold.foldl f (f (Fold.foldl f ac left) val) right
