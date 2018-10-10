# [Minimum spanning tree (MST) ](https://en.wikipedia.org/wiki/Minimum_spanning_tree)


A minimum spanning tree (MST) or minimum weight spanning tree is a subset of the edges of a connected, edge-weighted (un)directed graph that connects all the vertices together, without any cycles and with the minimum possible total edge weight.

 ## [Prim's algorithm](http://www.mathcs.emory.edu/~cheung/Courses/171/Syllabus/11-Graph/prim2.html)


``` 

ReachSet = {0};                    // You can use any node...
   UnReachSet = {1, 2, ..., N-1};
   SpanningTree = {};

   while ( UnReachSet ≠ empty )
   {
      Find edge e = (x, y) such that:
         1. x ∈ ReachSet
	 2. y ∈ UnReachSet
	 3. e has smallest cost

      SpanningTree = SpanningTree ∪ {e};

      ReachSet   = ReachSet ∪ {y};
      UnReachSet = UnReachSet - {y};
   }

```
