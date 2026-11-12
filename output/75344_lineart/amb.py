from check_geometry import *
import itertools
def show(ta,tb):
    for a in P:
        for b in P:
            if a.split('#')[0]==ta and b.split('#')[0]==tb and a<b or (ta==tb and a<b and a.split('#')[0]==ta and b.split('#')[0]==tb):
                o=rel(a,b)
                if min(o[0],o[1],o[2])>-6: print(ta,tb,a,b,[round(v,1) for v in o],kind(o),'kernel' if (a,b) in E or (b,a) in E else '')
for ta,tb in [('11458','15068'),('23950','32064a'),('27925','36840'),('41682','76766'),('41682','85984'),('11458','11458'),('44861','44861')]: show(ta,tb)
for n in ['11458#1','11458#3','15068#1','23950#1','32064a#1','41682#1','76766#1','85984#1','36840#1','27925#1']:
    p=P[n]; print(n,[round(v) for v in p['lo']],[round(v) for v in p['hi']])
