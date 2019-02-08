type foo = Foo of bar
and bar = Bar of foo

type 'a tree = Tree of 'a tree * 'a tree | Leaf of 'a

type suit = Spades | Hearts | Clubs | Diamonds
