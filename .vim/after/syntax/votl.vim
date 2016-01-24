syntax match votlTag "\v\#\w*" contained
highlight def votlTag term=bold ctermfg=Yellow
syntax match votlContext "\v\@\w*" contained
highlight def votlContext term=bold ctermfg=Blue

" Headings
syntax region OL1 start=+^[^:\t]+ end=+^[^:\t]+me=e-1 contains=votlTag,votlContext,outlTags,BT1,BT2,PT1,PT2,TA1,TA2,UT1,UT2,UB1,UB2,spellErr,SpellErrors,BadWord,OL2 keepend
syntax region OL2 start=+^\t[^:\t]+ end=+^\t[^:\t]+me=s-1 contains=votlTag,votlContext,outlTags,BT2,BT3,PT2,PT3,TA2,TA3,UT2,UT3,UB2,UB3,spellErr,SpellErrors,BadWord,OL3 keepend
syntax region OL3 start=+^\(\t\)\{2}[^:\t]+ end=+^\(\t\)\{2}[^:\t]+me=e-3 contains=votlTag,votlContext,outlTags,BT3,BT4,PT3,PT4,TA3,TA4,UT3,UT4,UB3,UB4,spellErr,SpellErrors,BadWord,OL4 keepend
syntax region OL4 start=+^\(\t\)\{3}[^:\t]+ end=+^\(\t\)\{3}[^:\t]+me=e-4 contains=votlTag,votlContext,outlTags,BT4,BT5,PT4,PT5,TA4,TA5,UT4,UT5,UB4,UB5,spellErr,SpellErrors,BadWord,OL5 keepend
syntax region OL5 start=+^\(\t\)\{4}[^:\t]+ end=+^\(\t\)\{4}[^:\t]+me=e-5 contains=votlTag,votlContext,outlTags,BT5,BT6,PT5,PT6,TA5,TA6,UT5,UT6,UB5,UB6,spellErr,SpellErrors,BadWord,OL6 keepend
syntax region OL6 start=+^\(\t\)\{5}[^:\t]+ end=+^\(\t\)\{5}[^:\t]+me=e-6 contains=votlTag,votlContext,outlTags,BT6,BT7,PT6,PT7,TA6,TA7,UT6,UT7,UB6,UB7,spellErr,SpellErrors,BadWord,OL7 keepend
syntax region OL7 start=+^\(\t\)\{6}[^:\t]+ end=+^\(\t\)\{6}[^:\t]+me=e-7 contains=votlTag,votlContext,outlTags,BT7,BT8,PT7,PT8,TA7,TA8,UT7,UT8,UB7,UB8,spellErr,SpellErrors,BadWord,OL8 keepend
syntax region OL8 start=+^\(\t\)\{7}[^:\t]+ end=+^\(\t\)\{7}[^:\t]+me=e-8 contains=votlTag,votlContext,outlTags,BT8,BT9,PT8,PT9,TA8,TA9,UT8,UT9,UB8,UB9,spellErr,SpellErrors,BadWord,OL9 keepend
syntax region OL9 start=+^\(\t\)\{8}[^:\t]+ end=+^\(\t\)\{8}[^:\t]+me=e-9 contains=votlTag,votlContext,outlTags,BT9,PT9,TA9,UT9,UB9,spellErr,SpellErrors,BadWord keepend

