# Red-Black Tree in Haskell
## Introduction
Red–black tree is a specialised binary search tree data structure noted for fast storage and retrieval of ordered information. Compared to other self-balancing binary search trees, the nodes in a red-black tree hold an extra bit called "color" representing "red" and "black" which is used when re-organising the tree to ensure that it is always approximately balanced.

| Operation | Amortized      | Worst Case |
| ---:      | ---:           | ---:       |
| Search    | O(logn)        | O(logn)    |
| Insert    | O(1)           | O(logn)    |
| Delete    | O(1)           | O(logn)    |


In addition to the requirements imposed on a binary search tree the following must be satisfied by a red–black tree:

1. Every node is either red or black.
2. All NIL nodes (figure 1) are considered black.
3. A red node does not have a red child.
4. Every path from a given node to any of its descendant NIL nodes goes through the same number of black nodes.
## Brief description of the program
The program relies on two main types of data:
~~~ haskell
data Color = RED | BLACK deriving (Show, Eq) 
data Tree a = NIL | Node (Tree a) Color a (Tree a) | DoubleBlackNode (Tree a) deriving (Show,Eq)
~~~ 
The date type color denotes the color of a particular node in the tree. Date type Tree can be ```NIL```, Regular node and Double black node. This serves to more conveniently restore invariants when a node is removed. While a regular node can have two colors, ```NIL``` and ```DoubleBlackNode``` are black by default.

The program has the following functionality:
* ```insert``` - insert value into tree
* ```delete``` - delete value from tree
* ```multiple_insert``` - insert multiple values into the tree
* ```multiple_delete``` - delete multiple values from the tree
* ```contains``` - check if there is a value in the tree
* ```create_tree``` - create a tree
* ```display``` - display a tree

## Getting Started
To start working in the program, you need to go to the src directory and enter in the terminal

```
ghci RedBlackTree.hs
```

Further, in order to work with the tree, it must be created. There are two ways to create a tree:
1. Step by step creation of a tree by manually inserting elements into it. That is, if we want to create a tree from the elements [1,2,3], then the creation of a tree can look like this:
     1. ```insert (insert (insert NIL 1) 2) 3```.
     2. Result: ```Node (Node NIL RED 1 NIL) BLACK 2 (Node NIL RED 3 NIL)```.
2. create_tree (list of integers)
     1. The only argument is a list of integers that form the tree. The function works in such a way that the insert function is called internally as many times as there is a value in the list.
     2. Example: ```create_tree [1,2,3,4,5]```.

## Tree functionality

* ```insert``` takes two arguments: a tree and a value to insert. Example: ```insert (Node NIL BLACK 1 NIL) 5```. The result will be: ```Node NIL BLACK 1 (Node NIL RED 5 NIL)```.
* ```delete``` takes two arguments: a tree and a value to delete. Example: ```delete (Node NIL BLACK 1 (Node NIL RED 5 NIL)) 5```. The result will be: ```Node NIL BLACK 1 NIL```.
* ```multiple_insert```. takes a tree and a list of values to be inserted. Example: ```multiple_insert (Node NIL BLACK 1 NIL) [2,3]```. Result: ```Node (Node NIL RED 1 NIL) BLACK 2 (Node NIL RED 3 NIL)```.
* ```contains```. takes a tree and a value to be checked for presence in the tree. Example: ```contains (Node (Node NIL RED 1 NIL) BLACK 2 (Node NIL RED 3 NIL)) 1```. The result is ```True```.
* ```display```. takes a tree and returns its visualization. Example: ```display (create_tree [1,2,3,4,5,6,7,8,9,10])```. Result:
```                ┌──10(RED)
            ┌──9(BLACK)
        ┌──8(RED)
        |   └──7(BLACK)
    ┌──6(BLACK)
    |   └──5(BLACK)
───4(BLACK)
    |   ┌──3(BLACK)
    └──2(BLACK)
        └──1(BLACK)
```
