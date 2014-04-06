let                      
    subj = asScore $ scat [ c,       d,        f,          e           ]
    cs1  = asScore $ scat [ g,f,e,g, f,a,g,d', c',b,c',d', e',g',f',e' ]
in compress 4 cs1 </> subj
