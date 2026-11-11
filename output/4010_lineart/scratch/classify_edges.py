import csv, math, sys
inst=list(csv.DictReader(open('scratch/4010_inst_dump.csv')))
cnt={}; P={}
for x in inst:
    k=x['dat']; cnt[k]=cnt.get(k,0)+1; n=f"{k}#{cnt[k]}"
    R=[float(v) for v in x['R'].split()]  # column-major from R: R[1,1],R[2,1],R[3,1],R[1,2]...
    M=[[R[0],R[3],R[6]],[R[1],R[4],R[7]],[R[2],R[5],R[8]]]
    w,d,h=float(x['w']),float(x['d']),float(x['h'])
    ex=abs(M[0][0])*w/2+abs(M[0][2])*d/2; ez=abs(M[2][0])*w/2+abs(M[2][2])*d/2
    ey=h  # y spans y..y+h
    P[n]=dict(dat=k,step=int(x['step']),x=float(x['x']),y=float(x['y']),z=float(x['z']),ex=ex,ez=ez,h=h,nm=x['nm'],grp=x['grp'])
rows=list(csv.reader(open('4010_adjacency_matrix.csv')))
names=[r.strip('"') for r in rows[0][1:]]
E=[]
for i,r in enumerate(rows[1:]):
    for j,v in enumerate(r[1:]):
        if j>i and float(v)>0: E.append((names[i],names[j]))
def cls(a,b):
    A,B=P.get(a),P.get(b)
    if not A or not B: return 'manual',0,0,0
    ox=min(A['x']+A['ex'],B['x']+B['ex'])-max(A['x']-A['ex'],B['x']-B['ex'])
    oz=min(A['z']+A['ez'],B['z']+B['ez'])-max(A['z']-A['ez'],B['z']-B['ez'])
    oy=min(A['y']+A['h'],B['y']+B['h'])-max(A['y'],B['y'])
    return ('vert' if (ox>1 and oz>1 and oy<=3) else ('inside' if (ox>1 and oz>1 and oy>3) else 'lateral')),round(ox),round(oz),round(oy)
out=[]
for a,b in E:
    c,ox,oz,oy=cls(a,b); out.append((a,b,c,ox,oz,oy))
import collections
print(collections.Counter(o[2] for o in out))
w=csv.writer(open('scratch/4010_edge_classes.csv','w',newline=''))
w.writerow(['a','b','class','ox','oz','oy','stepA','stepB','nameA','nameB'])
for a,b,c,ox,oz,oy in out:
    A,B=P.get(a,{}),P.get(b,{})
    w.writerow([a,b,c,ox,oz,oy,A.get('step'),B.get('step'),A.get('nm'),B.get('nm')])

print("=== VERT")
import collections
g=collections.defaultdict(list)
for a,b,c,ox,oz,oy in out:
    if c=='vert':
        k=tuple(sorted([a.split('#')[0],b.split('#')[0]]))
        g[k].append(f"{a}-{b}[{ox},{oz},{oy}]")
for k,v in sorted(g.items()): print(k,len(v),' '.join(v[:5]))
print("=== MISSING candidates (footprint overlap, vertical contact within 8, not in matrix)")
S=set(frozenset(e) for e in E)
ns=[n for n in P]
for i,a in enumerate(ns):
    for b in ns[i+1:]:
        if frozenset((a,b)) in S: continue
        A,B=P[a],P[b]
        ox=min(A['x']+A['ex'],B['x']+B['ex'])-max(A['x']-A['ex'],B['x']-B['ex'])
        oz=min(A['z']+A['ez'],B['z']+B['ez'])-max(A['z']-A['ez'],B['z']-B['ez'])
        if ox>1 and oz>1:
            g1=A['y']+A['h']-B['y']; g2=B['y']+B['h']-A['y']
            if min(abs(g1),abs(g2))<=8: print(a,b,round(ox),round(oz),round(g1),round(g2),A['nm'][:18],'|',B['nm'][:18])
