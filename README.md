Tic-Tac-Toe
===========

Tic-Tac-Toe game made with Mathematica/the Wolfram Language. The AI uses the Minimax algorithm, so it's unbeatable. Alpha-beta pruning makes it fast.

Output
==========

<img src="https://dl.dropboxusercontent.com/u/2736911/tictactoe.gif" alt="Output in a Mathematica notebook" title="Playing a few games against the AI..." />

Code
=========
	listChildren[board_, player_] := listChildren[board, player] = ReplaceList[board, {a___, {x___, 0, y___}, b___} :> {a, {x, player, y}, b}]
	score[board_] := With[{
	   score = {
	       Total /@ #,
	       Total /@ Transpose[#],
	       Tr[#],
	       Tr[Reverse@#]
	       } &[{{8, 1, 6}, {3, 5, 7}, {4, 9, 2}} board]
	   },
	  Which[
	   MemberQ[score, 15, 2], 1,
	   MemberQ[score, 30, 2], -1,
	   True, 0
	   ]
	  ]
	nextMove[oldBoard_] :=
	 If[
	  score[oldBoard] != 0 || ! MemberQ[oldBoard, 0, 2], 
	  oldBoard,
	  Part[
	   listChildren[oldBoard, 1],
	   Last@Ordering[
	     alphabeta[#, -10, 10, False] & /@ listChildren[oldBoard, 1]]
	   ]
	  ]
	alphabeta[node_, a_, b_, maximizingPlayer_] := 
	 Module[{alpha = a, beta = b, children},
	  children = listChildren[node, If[maximizingPlayer, 1, 2]];
	  If[Length@children == 0 || score@node != 0, Return@score@node];
	  If[maximizingPlayer,
	   Do[
	    alpha = Max[alpha, alphabeta[child, alpha, beta, False]];
	    If[beta <= alpha, Break[]];
	    , {child, children}];
	   Return[alpha],
	   Do[
	    beta = Min[beta, alphabeta[child, alpha, beta, True]];
	    If[beta <= alpha, Break[]];
	    , {child, children}];
	   Return[beta]
	   ]
	  ]
	circle = Graphics[Circle[]];
	cross = Graphics[{
	    Thick,
	    Line[{
	      {{-1, 1}, {1, -1}},
	      {{1, 1}, {-1, -1}}
	      }]
	    }];
	empty = Graphics[];
	hover = Graphics[{}, Background -> Orange];
	button[{i_, j_}] := Button[
	  Mouseover[
	   empty,
	   MouseAppearance[hover, "LinkHand"]
	   ],
	  currentBoard = nextMove[ReplacePart[currentBoard, {i, j} -> 2]],
	  Appearance -> None
	  ]
	renderBoard[board_] := GraphicsGrid[
	  Normal@SparseArray[
	      Join[
	       # -> cross & /@ Position[#, 1],
	       # -> circle & /@ Position[#, 2],
	       {{i_, j_} /; Extract[#, {i, j}] == 0 :> button[{i, j}]}
	       ], {3, 3}
	      ] &[board], Frame -> All, ImageSize -> 400
	  ]
	currentBoard = {{0, 0, 0}, {0, 0, 0}, {0, 0, 0}};
	Dynamic@Deploy@Overlay[{
	    renderBoard[currentBoard], 
	    If[score[currentBoard] != 0 || ! MemberQ[currentBoard, 0, 2], 
	     Pane[Column[{
	        Button["Go first", currentBoard = {{0, 0, 0}, {0, 0, 0}, {0, 0, 0}}],
	        Button["Go second", currentBoard = {{1, 0, 0}, {0, 0, 0}, {0, 0, 0}}]
	        }], ImageMargins -> 155], ## &[]]
	    }, {1, 2}, If[score[currentBoard] != 0 || ! MemberQ[currentBoard, 0, 2], 2, 1]]
