"""Per-set configuration, read from set_config.yaml in the current directory (output/<set>_lineart/)."""
import yaml, os
C = yaml.safe_load(open(os.environ.get('LEGO_SET_CONFIG', 'set_config.yaml')))
SET = str(C['set'])
F = {k: v.format(set=SET) for k, v in {
    'instances': '{set}_instances.csv', 'nodes': '{set}_nodes.csv', 'bbox': 'part_bbox.csv',
    'kernel': '{set}_adjacency_matrix.csv', 'checked': '{set}_adjacency_matrix_checked.csv',
    'audit': '{set}_link_audit.csv', 'summary': '{set}_link_audit_summary.md',
    'compat': '../../data/lego_compat_out/compat_matrix_subtype_codes.csv',
    **(C.get('files') or {})}.items()}
SUBTYPE_OVERRIDE = {str(k): v for k, v in (C.get('subtype_override') or {}).items()}
PAIR_ALLOW = {frozenset((str(a), str(b))): j for a, b, j in (C.get('pair_allow') or [])}
FLEXIBLE = set(str(p) for p in (C.get('flexible_parts') or []))
MANUAL_ADD = [(str(a), str(b), j, why) for a, b, j, why in (C.get('manual_add') or [])]   # node names, e.g. "3829a#1"
MANUAL_REMOVE = [(str(a), str(b), why) for a, b, why in (C.get('manual_remove') or [])]   # false positives confirmed by hand: [node_a, node_b, reason]
BRIDGE = bool(C.get('bridge_subassemblies', False))   # re-join sub-assemblies cut off by the box rules (10174)
TOL = float(C.get('candidate_tol_ldu', 6.0))
