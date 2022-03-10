p(X,Z) :- q(X,Y), p(Y,Z).
p(X,X).
q(a,b).

append([],Ys,Ys).
append(.(X,Xs),Ys,.(X,Zs)) :- append(Xs,Ys,Zs).