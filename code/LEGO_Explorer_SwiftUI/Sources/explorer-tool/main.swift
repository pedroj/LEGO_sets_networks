import Foundation
import ExplorerCore

// explorer-tool classify <db>              check the Swift classifier against the categories stored in <db>
// explorer-tool build <csvDir> <out.sqlite>  build a database from Rebrickable CSV(.gz) files
// explorer-tool download <dir>             download the CSVs from Rebrickable
// explorer-tool info <db>

func fail(_ m: String) -> Never { FileHandle.standardError.write(Data((m + "\n").utf8)); exit(1) }
setvbuf(stdout, nil, _IOLBF, 0)
let a = CommandLine.arguments
guard a.count >= 3 else { fail("usage: explorer-tool classify|build|download|info …") }

switch a[1] {
case "classify":
    let db = try ExplorerDatabase(path: a[2])
    var bad = 0, n = 0
    let cls = PartClassifier()
    let t0 = Date()
    try db.db.query("SELECT part_num, part_name, part_category, category, subcategory FROM parts") { r in
        n += 1
        let got = cls.classify(name: r.optText(1), category: r.optText(2))
        if got.category != r.text(3) || got.subcategory != r.text(4) {
            bad += 1
            if bad <= 25 { print("MISMATCH \(r.text(0)) '\(r.text(1))' [\(r.text(2))]: R=\(r.text(3))/\(r.text(4))  Swift=\(got.category)/\(got.subcategory)") }
        }
    }
    print("\(n) parts classified in \(String(format: "%.1f", Date().timeIntervalSince(t0))) s, \(bad) mismatches")
    exit(bad == 0 ? 0 : 2)
case "build":
    guard a.count >= 4 else { fail("usage: explorer-tool build <csvDir> <out.sqlite>") }
    let t0 = Date()
    try TableBuilder().build(csvDir: URL(fileURLWithPath: a[2]), output: URL(fileURLWithPath: a[3]), source: a[2]) { msg, p in
        print(String(format: "[%3.0f%%] %@", p * 100, msg))
    }
    print(String(format: "built in %.1f s", Date().timeIntervalSince(t0)))
case "download":
    let dir = URL(fileURLWithPath: a[2])
    try await TableBuilder().download(to: dir) { msg, p in print(String(format: "[%3.0f%%] %@", p * 100, msg)) }
case "gunzip":
    var total = 0
    try Gzip.stream(file: URL(fileURLWithPath: a[2])) { total += $0.count }
    print("decompressed bytes: \(total)")
case "selftest":
    let db = try ExplorerDatabase(path: a[2])
    func time<T>(_ label: String, _ f: () -> T) -> T { let t = Date(); let r = f(); print(String(format: "%-34@ %6.1f ms", label as NSString, Date().timeIntervalSince(t) * 1000)); return r }
    let sets = time("load sets") { db.loadSets() }
    let parts = time("load parts") { db.loadParts() }
    print("  \(sets.count) sets, \(parts.count) parts; newest: \(sets[0].setNum) \(sets[0].name) (\(sets[0].year ?? 0))")
    let s = sets.first { $0.setNum == "75192-1" }!
    let lines = time("set 75192-1 parts list") { db.setLines(setID: s.id, includeSpares: false, includeMinifigParts: true, mergeColors: false) }
    let totals = db.partsTotals(lines)
    print("  \(s.name): \(totals.types) part types, \(totals.colours) colours, \(totals.pieces) pieces (LEGO lists \(s.numParts)); first: \(lines[0].category) › \(lines[0].subcategory) \(lines[0].partNum) \(lines[0].colorName) x\(lines[0].quantity)")
    let noMini = db.setLines(setID: s.id, includeSpares: false, includeMinifigParts: false, mergeColors: false)
    let withSpares = db.setLines(setID: s.id, includeSpares: true, includeMinifigParts: true, mergeColors: false)
    let merged = ExplorerDatabase.mergeColours(lines)
    print("  rows: default \(lines.count), no minifig parts \(noMini.count), with spares \(withSpares.count), colours merged \(merged.count)")
    let p = parts.first { $0.partNum == "3001" }!
    let cl = time("part 3001 colours") { db.colorLines(partID: p.id) }
    let sl = time("part 3001 sets") { db.setLines(partID: p.id) }
    print("  3001 \(p.name): \(p.category) › \(p.subcategory); \(cl.count) colours, \(sl.count) sets (index says \(p.nSets) sets)")
    let t = Date(); var n = 0
    for _ in 0..<20 { n += sets.filter { $0.nameLower.contains("falcon") }.count + parts.filter { $0.nameLower.contains("1 x 2") }.count }
    print(String(format: "filter sets+parts by name text: %.1f ms per keystroke", Date().timeIntervalSince(t) * 1000 / 20))
case "info":
    let db = try ExplorerDatabase(path: a[2])
    print(db.info())
default: fail("unknown command \(a[1])")
}
