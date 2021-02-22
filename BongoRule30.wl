(* ::Package:: *)

(* 
Author: Celestine P. Lawrence
Date: 22/02/21
Runtime: ~8 mins using Wolfram Mathematica 12.2 on a HP Slim Desktop S01-pF1000nd
Result: https://youtu.be/k12zioVQwwk , on combining the .mp3 and .avi with Windows video editor
*)	


t=1000; (*number of Bongo beats*)


(*Generate audio*)
dat=ResourceData["A Million Bits of the Center Column of the Rule 30 Cellular Automaton"][[1;;t]];
audio30=Sound[dat/.{1->SoundNote["HighBongo",.1],0->SoundNote["LowBongo",.1]}]
Export["ca30.mp3" ,Audio@audio30 ]


(*Generate video*)
c=CellularAutomaton[30,{{1},0},{{0,t-1}}];
Image[c]
pics=Table[ Image@ArrayPad[c[[;;n]],{{0,t-n},0}],{n,t}];
vid=VideoGenerator[pics,100]
Export["ca30.avi" ,vid ]
