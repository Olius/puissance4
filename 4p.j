w=: h=: 5 [ n=: 4 [ p=: 'AB' [ s=: ' '

play=: ( w $ < )&.>
pack=: -.&s"1 &.|: "2
tran=: ( ] |."0 1 w {."0 $ $ p"_ )"1
filt=: (,: $~ h >: #)&.>

line=: [: ,/ (1,n)        {.;._3 ]
diag=: [: ,/ (2$n) (<0 1)&|:;._3 ]
runs=: [: ; (line , diag)&.> @ (; |:)
winq=: p e."1 (}. -: }:)"1 # {."1
pwin=: winq @ runs "2

trim=: pwin&.> @ filt @: (pack @ tran @ ; &.>) L:2 @ {::

psum=: {"1 (+/ . * % +/@[) ]
cumu=: ,:@(psum ;) L:0 1
prob=: cumu^:(L.@] - 2:)

drop=: (= i.@$) #L:0&.> ]
