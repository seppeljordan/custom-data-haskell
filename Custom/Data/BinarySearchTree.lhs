> module Custom.Data.BinarySearchTree
>     ( Tree
>     , empty
>     , insert
>     , find
>     , remove
>     , fromList
>     )
> where

We want to make our Tree an instance of Foldalble, so we have to
import then module Data.Foldable.  Because a lot of the functions in
this module have the same names like functions in Prelude, we will
import it qualified.

> import qualified Data.Foldable as Fold

We will make our tree an instance of the monoid type class.  Thats why
we import Data.Monoid.

> import qualified Data.Monoid as Mono

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

We want to make our binary search tree data type an instance of the
monoid type class.

> instance (Ord a) => Mono.Monoid (Tree a) where

The neutral element of the monoid is the empty tree, because we an empty tree added to any tree x is x.

>       mempty = empty

Now we define what it means two "add" two trees.  We define the
mappend operation as merging two binary search trees into one.

>       mappend = mergeTrees

That leaves us with the question how we define the mergeTrees
operation.  We want to make this operation as stable as possible.  The
general idea of this is that we generate two sorted lists of the trees
we want to merge and then generate a new (balanced) tree from the two
lists.

> mergeTrees :: (Ord a) => Tree a -> Tree a -> Tree a
> mergeTrees t1 t2 =
>     treeFromSortedList (mergeSortedLists (Fold.toList t1)
>                                          (Fold.toList t2)
>                        )

Now we have to define what it means to merge two sorted lists.  The
merge operation is a function that takes two (sorted) lists and
returns one list that contains all elements of the two lists in
ascending order.

> mergeSortedLists :: (Ord a) => [a] -> [a] -> [a]

Note that we will not check if the lists are actually sorted.  A list
merged with an empty list is the list itself.

> mergeSortedLists xs [] = xs
> mergeSortedLists [] ys = ys

In general the merge of two lists is the smaller item of the two heads
of the lists followed by the merged lists without the selected
element.

> mergeSortedLists (x:xs) (y:ys)
>     | x <= y = x:(mergeSortedLists xs (y:ys))
>     | otherwise = y:(mergeSortedLists (x:xs) (ys))

Now we define what it means to construct a tree from a sorted list.
The contruction is a function that takes a list and returns a tree
containing the elements of the list.

> treeFromSortedList :: [a] -> Tree a

A tree contructed from an empty list is just a Leaf.

> treeFromSortedList [] = Leaf

In general our approach is two split the list in half where we select
the element in the middle of the list as the root node.  All elements
to the left of the pivot element are assumed to be smaller than the
pivot so they go to the left subtree and the elements to the right of
pivot got to the right of the root.

> treeFromSortedList xs =
>     (Branch pivot (treeFromSortedList lowerHalf)
>                   (treeFromSortedList upperHalf)
>     )
>     where pivot = xs !! (n `div` 2)
>           lowerHalf = take (n `div` 2) xs
>           upperHalf = drop ((n `div` 2) + 1) xs
>           n = length xs

We want to implement a function that takes an unordered list and
returns a tree.  An intuitive approach would be to fold a list into a
tree.  This would leed to a unbalanced tree if the list was already
sorted.  What we do instead is to sort the list with a sorting
algorithm and contruct a tree by applying treeFromSortedList.  This
approach leeds to an almost balanced tree.  A tree from an empty list
is a leaf.

> fromList :: (Ord a) => [a] -> Tree a
> fromList = treeFromSortedList.mergesort

Now we define our search algorithm mergesort.  We consider empty lists
and singleton lists to be ordered.

> mergesort :: (Ord a) => [a] -> [a]
> mergesort [] = []
> mergesort (x:[]) = [x]

In general we split our list into two sublists and mergesort them.

> mergesort xs =
>     mergeSortedLists
>     (mergesort firstSplit)
>     (mergesort secondSplit)
>     where (firstSplit, secondSplit) = split xs

So what is split?  We don't want to scan the length of our list
because this would cost us another itaration over the list.  Instead
we split the list into the odd and even positioned elements in the
list.

> split :: [a] -> ([a],[a])
> split xs = (odds xs, evens xs)
>     where evens [] = []
>           evens (x:[]) = []
>           evens (x:y:rest) = y : evens rest
>           odds [] = []
>           odds (x:xs) = x : evens xs
