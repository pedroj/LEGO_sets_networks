import pickle, collections, numpy as np
import check_geometry as G
acc = pickle.load(open('acc.pkl','rb')); rows = pickle.load(open('pairs.pkl','rb'))
N = len(G.names); deg = np.zeros(N,int)
for i,j,_,_ in acc: deg[i]+=1; deg[j]+=1
iso = [i for i in range(N) if deg[i]==0]
c = collections.Counter(G.dat[i] if hasattr(G,'dat') else '' for i in iso)
print(c.most_common(30))
cand = collections.defaultdict(list)
for i,j,k,cd,inK,o in rows:
    if deg[i]==0: cand[i].append((j,k,cd,inK,tuple(np.round(o,1))))
    if deg[j]==0: cand[j].append((i,k,cd,inK,tuple(np.round(o,1))))
nocand = [i for i in iso if not cand[i]]
print('isolated with no candidate pair', len(nocand), 'with candidates', len(iso)-len(nocand))
for i in iso[:12]:
    print(i, G.names[i], G.dat[i], [(G.dat[j],k,cd,inK,o) for j,k,cd,inK,o in cand[i][:4]])
