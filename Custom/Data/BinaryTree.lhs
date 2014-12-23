> module Custom.Data.BinaryTree
>     ( Tree
>     , empty
>     , insert
>     , find
>     , remove
>     )
> where

We want to make our Tree an instance of Foldalble, so we have to
import then module Data.Foldable.  Because a lot of the functions in
this module have the same names like functions in Prelude, we will
import it qualified.

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

The right fold of a Branch with a function 'f' is the fold of the left
subtree and the value itself used as arguments for 'f'.  The resulting
value is the starting value for the fold of the right subtree.

>     foldr f ac (Branch val left right) =
>         Fold.foldr f (f val (Fold.foldr f ac right)) left

The left fold of a branch is a right fold with the left and the right
subtree exchanged.

>     foldl _ ac Leaf = ac
>     foldl f ac (Branch val left right) =
>         Fold.foldl f (f (Fold.foldl f ac left) val) right

We will now define a remove operation for a tree.  We will only
implement this function for ordered tree as we need a way to identify
items.

> remove :: (Ord a) => Tree a -> a -> Tree a

We will split the problem of removing an item into two problems.
First we locate the item in the tree an then we remove the root node
of this tree.

> remove tree item =
>     doWithSubtree removeRoot tree item

The doWithSubtree takes three arguments:
    f - a function that takes a tree and returns a tree
    tree - the tree to be worked on
    item - the item that is the root of the subtree to be found
It returns a new tree.

> doWithSubtree :: (Ord a) => (Tree a -> Tree a) -> Tree a -> a -> Tree a

When we detect a node that is a leaf we can be sure that the tree does
not contain the item we are looking for.

> doWithSubtree _ Leaf _ = Leaf

Otherwise we have to check if we alread reached the node we are
looking for.

> doWithSubtree f (Branch x left right) item
>     | x == item = f (Branch x left right)

If we have not found the right node yet then we have to check the
subtrees of the current branch.  If x is smaller then the item we are
looking for then we know that we have to check the right subtree and
otherwise the left subtree.

>     | x > item = (Branch x (doWithSubtree f left item) right)
>     | otherwise = (Branch x left (doWithSubtree f right item))

We will now define what it means to remove the root of a tree.  The
removal of the root of a tree is a function that takes a tree and
returns a tree with the root removed.

> removeRoot :: Tree a -> Tree a

An empty tree with with its root removed is just an empty tree.

> removeRoot Leaf = Leaf

If our trees root has only one subtree then we can just make that
subtree the new root.

> removeRoot (Branch _ Leaf right) = right
> removeRoot (Branch _ left Leaf) = left

To remove the root of tree with two child our strategy is to change
the root with the leftest node of the right subtree and then remove
the node in that subtree.  This reduces the problem of an arbitrary
node to the problem of removing a node with zero or one subtrees.

> removeRoot (Branch _ left right@(Branch x subleft _)) =
>     (Branch nextInTree left (removeSmallest right))
>     where nextInTree = maybe x id (findSmallest subleft)

In the definition of removeRoot we used some functions that we haven't
defined yet, removeSmallest and findSmallest.

findSmallest finds the smallest element of a tree.  The smallest
element of a tree is the element most the left.  We can also use the
Foldable properties of our tree.  The toList function returns list
based on a left fold of the tree, so we know that the smallest element
of the tree is the head of this list.

> findSmallest :: Tree a -> Maybe a
> findSmallest tree =
>     case Fold.toList tree of
>       [] -> Nothing
>       x:_ -> Just x

removeSmallest can be defined in terms of removeRoot.  We will go to
the subtree that is most left and remove its root.

> removeSmallest :: Tree a -> Tree a
> removeSmallest Leaf = Leaf
> removeSmallest node@(Branch _ Leaf _) =
>     removeRoot node
> removeSmallest (Branch x left right) =
>     (Branch x (removeSmallest left) right)
