w=: h=: 5 [ n=: 4 [ p=: 1 2
b=: i.1,0,w
play=: [: ,/ ] ,"2 1/ (=i.w) * [
pack=: -.&0 "1 &.|: "2

iter=: (#~ (h >: #@pack)"2) @ play

line=: [: ,/ (1,n)        {.;._3 ]
diag=: [: ,/ (2$n) (<0 1)&|:;._3 ]
runs=: [: ; (line , diag)&.> @ (; |:)

winq=: (p e. ]) @: (({. * }. -: }:)"1) @ runs @ pack

tran=: - |."_1 w {."0 # $ p"_
winb=: winq @ tran &>
psum=: ( {"1 (+/ . * % +/@[) ] )"2
