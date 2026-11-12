from check_geometry import *
import csv
rows=list(csv.reader(open('75344-1_adjacency_matrix_edgelist.csv')))
cn=[c.strip('"') for c in rows[0][1:]]
PED=set()
for i,r in enumerate(rows[1:]):
    for j,v in enumerate(r[1:]):
        if float(v)>0: PED.add(frozenset((cn[i],cn[j])))
m=lambda t:'64567' if t=='577b' else t.rstrip('ab') if t in('3040b','3068b','3069b','4865b') else ('32064' if t=='32064a' else t)
# fix map: Rebrickable keeps letters
def rb(t):
    return {'577b':'64567'}.get(t,t)
print(len(PED), sorted(map(sorted,PED))[:5])
tp=lambda a,b: frozenset((rb(a.split('#')[0]),rb(b.split('#')[0])))
import collections
g=collections.defaultdict(list)
for a,b in E:
    k=kind(rel(a,b)); g[(k,tuple(sorted(tp(a,b))), tp(a,b) in PED)].append((a,b))
for key in sorted(g):
    if key[0] in('inside','side','near','gap') and key[2]: print('PED+',key[0],key[1],len(g[key]))
seen={frozenset(x[1]) for x in g}
print('PED pairs with no kernel link:',[sorted(p) for p in PED if p not in {k[1] and frozenset(k[1]) for k in g}])
cnt=collections.Counter((k[0],k[2]) for k in g for _ in g[k]); print(cnt)
print('-----')
keep=set()
def add(a,b): keep.add(frozenset((a,b)))
for a,b in E:
    k=kind(rel(a,b))
    if k=='stack' or (k=='inside' and tp(a,b) in PED): add(a,b)
for a,b in E:
    pass
# missing stacks
names=list(P)
Es=set(frozenset(e) for e in E)
for i,a in enumerate(names):
    for b in names[i+1:]:
        if frozenset((a,b)) not in Es and kind(rel(a,b))=='stack': add(a,b)
have=collections.Counter(tp(*tuple(k)) for k in keep)
rows2=list(csv.reader(open('75344-1_adjacency_matrix_edgelist.csv')))
for p in sorted(PED,key=lambda p:sorted(p)):
    l=sorted(p); w=have.get(p,0)
    pw=float(rows2[1+cn.index(l[0])][1+cn.index(l[-1])])
    if w==0: print('PED pair w/o kept link',l,'pedro_w',pw)
print('kept',len(keep))
nonPED=collections.Counter()
for k in keep:
    if tp(*tuple(k)) not in PED: nonPED[tuple(sorted(tp(*tuple(k))))]+=1
print('kept but not in PED:',dict(nonPED))
