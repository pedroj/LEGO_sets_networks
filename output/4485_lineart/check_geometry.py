import csv, itertools, collections, json
inst=list(csv.DictReader(open('4485_instances.csv')))
bb={r['part']:{k:float(r[k]) for k in r if k!='part'} for r in csv.DictReader(open('part_bbox.csv'))}
cnt={}; P={}
for x in inst:
    k=x['dat']; cnt[k]=cnt.get(k,0)+1; n=x['node']
    R=[float(v) for v in x['R'].split()]
    M=[[R[0],R[3],R[6]],[R[1],R[4],R[7]],[R[2],R[5],R[8]]]
    b=bb[k]; pts=[]
    for cx in (b['xmin'],b['xmax']):
        for cy in (b['ymin'],b['ymax']):
            for cz in (b['zmin'],b['zmax']):
                pts.append([M[r][0]*cx+M[r][1]*cy+M[r][2]*cz for r in range(3)])
    pos=[float(x['x']),float(x['y']),float(x['z'])]
    lo=[min(p[i] for p in pts)+pos[i] for i in range(3)]; hi=[max(p[i] for p in pts)+pos[i] for i in range(3)]
    P[n]=dict(dat=k,step=0,lo=lo,hi=hi,nm=x['nm'],grp=x['grp'],col=x['col'])
def rel(a,b):
    A,B=P[a],P[b]
    o=[min(A['hi'][i],B['hi'][i])-max(A['lo'][i],B['lo'][i]) for i in range(3)]
    return o  # overlap (negative = gap)
def kind(o):
    ox,oy,oz=o
    if ox>1 and oz>1:
        if -1<=oy<=5.5: return 'stack'
        if oy>5.5: return 'inside'
        return 'gap'
    if oy>5.5 and ((ox>1 and -1.5<=oz<=1.5) or (oz>1 and -1.5<=ox<=1.5)): return 'side'
    if oy>5.5 and ox>1 and oz>1: return 'inside'
    return 'near'
rows=list(csv.reader(open('4485_adjacency_matrix_kernel.csv')))
names=[r.strip('"') for r in rows[0][1:]]
E=[(names[i],names[j]) for i,r in enumerate(rows[1:]) for j,v in enumerate(r[1:]) if j>i and float(v)>0]
pass
if __name__=='__main__':
    res=[]
    for a,b in E:
        if a in P and b in P:
            o=rel(a,b); res.append((a,b,kind(o),[round(v,1) for v in o]))
    print(collections.Counter(r[2] for r in res))
    for kd in ('stack','side','inside','near','gap'):
        print('=====',kd)
        g=collections.defaultdict(list)
        for a,b,k,o in res:
            if k==kd:
                key=tuple(sorted([a.split('#')[0],b.split('#')[0]])); g[key].append(f"{a}-{b}{o}")
        for key,v in sorted(g.items()): print(key,len(v),' '.join(v[:3]))
