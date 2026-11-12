from check_geometry import *
Es=set(frozenset(e) for e in E)
names=list(P)
out=[]
for i,a in enumerate(names):
    for b in names[i+1:]:
        if frozenset((a,b)) in Es: continue
        o=rel(a,b)
        if kind(o)=='stack': out.append((a,b,[round(v,1) for v in o]))
print(len(out))
for r in out: print(*r)
