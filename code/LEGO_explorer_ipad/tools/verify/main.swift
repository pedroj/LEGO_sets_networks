import Foundation
// Compares PartClassifier against the categories stored in lego.sqlite (from parts_index.rds).
let path = CommandLine.arguments.count > 1 ? CommandLine.arguments[1] : "LEGOExplorer/Resources/lego.sqlite"
let db = try SQLiteDB(path: path, readonly: true)
let rows = try db.query("SELECT part_num, part_name, part_category, category, subcategory FROM parts") {
    ($0.text(0), $0.text(1), $0.optText(2) ?? "NA", $0.text(3), $0.text(4))
}
var bad = 0
let t0 = Date()
for r in rows {
    let c = PartClassifier.classify(name: r.1, category: r.2)
    if c.category != r.3 || c.subtype != r.4 {
        bad += 1
        if bad <= 15 { print("MISMATCH \(r.0) '\(r.1)' [\(r.2)]: swift=\(c.category)/\(c.subtype) R=\(r.3)/\(r.4)") }
    }
}
print("Checked \(rows.count) parts in \(String(format: "%.1f", Date().timeIntervalSince(t0))) s: \(bad) mismatches")
exit(bad == 0 ? 0 : 1)
