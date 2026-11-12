import pickle, collections, numpy as np, re
import check_geometry as G
rows = pickle.load(open('pairs.pkl', 'rb'))
dat, names = G.dat, G.names
NONSTUD = re.compile(r'pin|axle|bar|clip|ball|hinge|gear|wheel|door|track|turntable|minifig')

def engaged(o, cd):
    """-> (decision, reason). o = overlaps (ox,oy,oz) in LDU; cd = compatibility code of the two subtypes"""
    if cd == '': return False, 'compatibility rules: these part types cannot join'
    o = np.asarray(o)
    if 'stud' in cd:
        for a in range(3):                      # stud engagement along any axis (SNOT included)
            others = [o[b] for b in range(3) if b != a]
            if 2 <= o[a] <= 5.5 and min(others) >= 8:   # real stud insertion (stud height 4); flush contact (o=0) is not a joint
                return True, 'stud engagement: boxes meet along axis %s with %.1f LDU stud insertion and footprint >= 8' % ('xyz'[a], o[a])
    rest = cd.replace('stud+', '').replace('+stud', '')
    if NONSTUD.search(rest) and rest != 'stud':
        if (o >= -1.5).all() and (o >= 4).sum() >= 2:
            return True, 'non-stud joint (%s): boxes interpenetrate/touch' % rest
    return False, 'no engagement in the real geometry'

if __name__ == '__main__':
    acc = []; cnt = collections.Counter()
    for i, j, k, cd, inK, o in rows:
        ok, why = engaged(o, cd)
        cnt[(ok, inK)] += 1
        if ok: acc.append((i, j, why, inK))
    print(cnt)
    N = len(names); deg = np.zeros(N, int); earlier = np.zeros(N, int)
    for i, j, _, _ in acc: deg[i] += 1; deg[j] += 1; earlier[j] += 1
    print('links', len(acc), 'isolated', (deg == 0).sum(), 'parts without an earlier link', (earlier == 0).sum())
    # components
    par = list(range(N))
    def f(x):
        while par[x] != x: par[x] = par[par[x]]; x = par[x]
        return x
    for i, j, _, _ in acc: par[f(i)] = f(j)
    comp = collections.Counter(f(i) for i in range(N))
    print('components', len(comp), sorted(comp.values(), reverse=True)[:15])
    print('degree dist', np.bincount(np.minimum(deg, 12)))
    pickle.dump(acc, open('acc.pkl', 'wb'))
