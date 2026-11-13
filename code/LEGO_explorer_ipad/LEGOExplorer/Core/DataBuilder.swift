import Foundation

/// Port of build_data.R: turns Rebrickable's CSV downloads into the app's SQLite database.
/// Runs entirely on the device, no API key needed.
enum DataBuilder {
    static let requiredTables = ["themes", "colors", "part_categories", "parts", "sets", "inventories", "inventory_parts"]
    static let optionalTables = ["inventory_minifigs"]
    static func downloadURL(_ table: String) -> URL {
        URL(string: "https://cdn.rebrickable.com/media/downloads/\(table).csv.gz")!
    }

    private enum Kind { case int, text }
    /// (csv column, sql column, kind)
    private struct Col { let csv: String; let kind: Kind }

    private static func findTable(_ dir: URL, _ t: String) -> URL? {
        for ext in ["csv.gz", "csv"] {
            let u = dir.appendingPathComponent("\(t).\(ext)")
            if FileManager.default.fileExists(atPath: u.path) { return u }
        }
        return nil
    }

    /// Streams a CSV file into `stg.<table>`; `cols` are matched to the header by name.
    private static func importTable(_ db: SQLiteDB, table: String, file: URL, cols: [Col]) throws {
        let decl = cols.map { "\($0.csv) \($0.kind == .int ? "INTEGER" : "TEXT")" }.joined(separator: ",")
        try db.exec("CREATE TABLE stg.\(table)(\(decl))")
        let ins = try db.prepare("INSERT INTO stg.\(table) VALUES (\(Array(repeating: "?", count: cols.count).joined(separator: ",")))")
        var idx: [Int]? = nil
        try db.exec("BEGIN")
        try CSVParser.parse(file: file) { row in
            guard let map = idx else {
                idx = try cols.map { c in
                    guard let i = row.firstIndex(of: c.csv) else {
                        throw DBError(message: "Column \(c.csv) missing in \(table).csv")
                    }
                    return i
                }
                return
            }
            var args: [DBValue] = []
            args.reserveCapacity(cols.count)
            for (k, c) in cols.enumerated() {
                let i = map[k]
                let v = i < row.count ? row[i] : ""
                switch c.kind {
                case .int: args.append(v.isEmpty ? .null : (Int(v).map { .int($0) } ?? .null))
                case .text: args.append(.text(v))
                }
            }
            try ins.run(args)
        }
        try db.exec("COMMIT")
    }

    /// Builds `output` from the CSV tables in `sourceDir`.
    static func build(sourceDir: URL, output: URL, sourceLabel: String,
                      progress: (String, Double) -> Void) throws {
        let fm = FileManager.default
        let stagePath = output.deletingLastPathComponent().appendingPathComponent("stage-\(UUID().uuidString).sqlite").path
        try? fm.removeItem(at: output)
        defer { try? fm.removeItem(atPath: stagePath) }

        let missing = requiredTables.filter { findTable(sourceDir, $0) == nil }
        guard missing.isEmpty else { throw DBError(message: "Missing tables: \(missing.joined(separator: ", "))") }

        let db = try SQLiteDB(path: output.path)
        try db.exec("PRAGMA journal_mode=OFF; PRAGMA synchronous=OFF; PRAGMA temp_store=FILE;")
        let esc = stagePath.replacingOccurrences(of: "'", with: "''")
        try db.exec("ATTACH DATABASE '\(esc)' AS stg")
        try db.exec("PRAGMA stg.journal_mode=OFF; PRAGMA stg.synchronous=OFF;")
        try db.exec(Schema.tables)

        // ---- read the CSV tables --------------------------------------------------
        let imports: [(String, [Col])] = [
            ("themes", [Col(csv: "id", kind: .int), Col(csv: "name", kind: .text), Col(csv: "parent_id", kind: .int)]),
            ("colors", [Col(csv: "id", kind: .int), Col(csv: "name", kind: .text), Col(csv: "rgb", kind: .text), Col(csv: "is_trans", kind: .text)]),
            ("part_categories", [Col(csv: "id", kind: .int), Col(csv: "name", kind: .text)]),
            ("parts", [Col(csv: "part_num", kind: .text), Col(csv: "name", kind: .text), Col(csv: "part_cat_id", kind: .int), Col(csv: "part_material", kind: .text)]),
            ("sets", [Col(csv: "set_num", kind: .text), Col(csv: "name", kind: .text), Col(csv: "year", kind: .int), Col(csv: "theme_id", kind: .int), Col(csv: "num_parts", kind: .int), Col(csv: "img_url", kind: .text)]),
            ("inventories", [Col(csv: "id", kind: .int), Col(csv: "version", kind: .int), Col(csv: "set_num", kind: .text)]),
            ("inventory_parts", [Col(csv: "inventory_id", kind: .int), Col(csv: "part_num", kind: .text), Col(csv: "color_id", kind: .int), Col(csv: "quantity", kind: .int), Col(csv: "is_spare", kind: .text), Col(csv: "img_url", kind: .text)]),
            ("inventory_minifigs", [Col(csv: "inventory_id", kind: .int), Col(csv: "fig_num", kind: .text), Col(csv: "quantity", kind: .int)]),
        ]
        var haveMinifigs = false
        for (k, (t, cols)) in imports.enumerated() {
            guard let f = findTable(sourceDir, t) else {
                if optionalTables.contains(t) {
                    try db.exec("CREATE TABLE stg.inventory_minifigs(inventory_id INTEGER, fig_num TEXT, quantity INTEGER)")
                    continue
                }
                throw DBError(message: "Missing table \(t)")
            }
            progress("Reading \(t) …", 0.30 + 0.35 * Double(k) / Double(imports.count))
            do { try importTable(db, table: t, file: f, cols: cols) }
            catch where optionalTables.contains(t) { // damaged optional table: carry on without it
                try? db.exec("DROP TABLE IF EXISTS stg.\(t)")
                try db.exec("CREATE TABLE stg.\(t)(inventory_id INTEGER, fig_num TEXT, quantity INTEGER)")
                continue
            }
            if t == "inventory_minifigs" { haveMinifigs = true }
        }

        // ---- themes: top-level theme (theme group) --------------------------------
        progress("Themes …", 0.66)
        let themes = try db.query("SELECT id, parent_id FROM stg.themes") { ($0.int(0), $0.optInt(1)) }
        let parent = Dictionary(themes.map { ($0.0, $0.1) }, uniquingKeysWith: { a, _ in a })
        try db.exec("ALTER TABLE stg.themes ADD COLUMN group_id INTEGER")
        let upd = try db.prepare("UPDATE stg.themes SET group_id=? WHERE id=?")
        try db.exec("BEGIN")
        for (id, _) in themes {
            var root = id, hops = 0
            while let p = parent[root] ?? nil, parent[p] != nil, hops < 50 { root = p; hops += 1 }
            try upd.run([.int(root), .int(id)])
        }
        try db.exec("COMMIT")

        // ---- indexes + latest inventory of every set ---------------------------------
        progress("Indexing …", 0.68)
        try db.exec("""
        CREATE INDEX stg.ip_inv ON inventory_parts(inventory_id);
        CREATE INDEX stg.inv_set ON inventories(set_num);
        CREATE INDEX stg.im_inv ON inventory_minifigs(inventory_id);
        CREATE TABLE stg.latest_inv(set_num TEXT PRIMARY KEY, inventory_id INTEGER);
        INSERT OR IGNORE INTO stg.latest_inv
            SELECT i.set_num, i.id FROM stg.inventories i
            WHERE i.version = (SELECT MAX(j.version) FROM stg.inventories j WHERE j.set_num = i.set_num);
        """)

        let spare = "CASE WHEN upper(substr(ip.is_spare,1,1)) IN ('T','1') THEN 1 ELSE 0 END"
        progress("Joining parts to sets …", 0.70)
        try db.exec("""
        CREATE TABLE stg.sp(set_num TEXT, part_num TEXT, color_id INTEGER, quantity INTEGER, is_spare INTEGER, fig_num TEXT, img TEXT);
        INSERT INTO stg.sp
            SELECT li.set_num, ip.part_num, ip.color_id, ip.quantity, \(spare), NULL, COALESCE(ip.img_url,'')
            FROM stg.latest_inv li JOIN stg.sets s ON s.set_num = li.set_num
            JOIN stg.inventory_parts ip ON ip.inventory_id = li.inventory_id;
        """)
        if haveMinifigs {
            progress("Adding minifigure parts …", 0.74)
            try db.exec("""
            INSERT INTO stg.sp
                SELECT li.set_num, ip.part_num, ip.color_id, ip.quantity * im.quantity, \(spare), im.fig_num, COALESCE(ip.img_url,'')
                FROM stg.latest_inv li JOIN stg.sets s ON s.set_num = li.set_num
                JOIN stg.inventory_minifigs im ON im.inventory_id = li.inventory_id
                JOIN stg.latest_inv fi ON fi.set_num = im.fig_num
                JOIN stg.inventory_parts ip ON ip.inventory_id = fi.inventory_id;
            """)
        }
        try db.exec("CREATE INDEX stg.sp_part ON sp(part_num)")

        // ---- classify parts (classify_part.R) --------------------------------------------
        progress("Classifying parts …", 0.80)
        try db.exec("CREATE TABLE stg.pcls(part_num TEXT PRIMARY KEY, category TEXT, subcategory TEXT)")
        let src = try db.query("""
            SELECT p.part_num, p.name, c.name FROM stg.parts p LEFT JOIN stg.part_categories c ON c.id = p.part_cat_id
            """) { ($0.text(0), $0.text(1), $0.optText(2) ?? "NA") }
        let insC = try db.prepare("INSERT OR IGNORE INTO stg.pcls VALUES (?,?,?)")
        try db.exec("BEGIN")
        for (pn, name, cat) in src {
            let r = PartClassifier.classify(name: name, category: cat)
            try insC.run([.text(pn), .text(r.category), .text(r.subtype)])
        }
        try db.exec("COMMIT")

        // ---- usage statistics + representative picture ------------------------------------
        progress("Counting usage …", 0.86)
        try db.exec("""
        CREATE TABLE stg.use AS SELECT part_num, COUNT(DISTINCT set_num) n_sets, COUNT(DISTINCT color_id) n_colors, SUM(quantity) total_qty
            FROM stg.sp GROUP BY part_num;
        CREATE INDEX stg.use_p ON use(part_num);
        CREATE TABLE stg.rep_img AS
            SELECT part_num, img FROM (
                SELECT part_num, img, ROW_NUMBER() OVER (PARTITION BY part_num ORDER BY COUNT(*) DESC, img) rn
                FROM stg.sp WHERE img <> '' GROUP BY part_num, img) WHERE rn = 1;
        CREATE INDEX stg.rep_p ON rep_img(part_num);
        """)

        // ---- final tables --------------------------------------------------------------------
        progress("Writing tables …", 0.90)
        try db.exec("""
        INSERT INTO sets(set_num, set_name, year, theme_id, num_parts, set_img_url, theme_name, theme_group)
            SELECT s.set_num, s.name, s.year, s.theme_id, s.num_parts, s.img_url, t.name, g.name
            FROM stg.sets s LEFT JOIN stg.themes t ON t.id = s.theme_id LEFT JOIN stg.themes g ON g.id = t.group_id
            ORDER BY s.set_num COLLATE BINARY;
        INSERT INTO colors SELECT id, name, rgb, CASE WHEN upper(substr(is_trans,1,1)) IN ('T','1') THEN 1 ELSE 0 END FROM stg.colors;
        INSERT INTO parts(part_num, part_name, category, subcategory, part_category, part_cat_id, part_material, part_img_url, n_sets, n_colors, total_qty)
            SELECT p.part_num, p.name, k.category, k.subcategory, c.name, p.part_cat_id, p.part_material,
                   COALESCE(r.img,''), COALESCE(u.n_sets,0), COALESCE(u.n_colors,0), COALESCE(u.total_qty,0)
            FROM stg.parts p JOIN stg.pcls k ON k.part_num = p.part_num
            LEFT JOIN stg.part_categories c ON c.id = p.part_cat_id
            LEFT JOIN stg.use u ON u.part_num = p.part_num
            LEFT JOIN stg.rep_img r ON r.part_num = p.part_num
            ORDER BY COALESCE(u.n_sets,0) DESC, p.part_num COLLATE BINARY;
        CREATE TABLE stg.sid AS SELECT set_num || '' AS set_num, set_id FROM sets;
        CREATE TABLE stg.pid AS SELECT part_num || '' AS part_num, part_id FROM parts;
        CREATE INDEX stg.sid_i ON sid(set_num);
        CREATE INDEX stg.pid_i ON pid(part_num);
        INSERT INTO figs(fig_num) SELECT DISTINCT fig_num FROM stg.sp WHERE fig_num IS NOT NULL ORDER BY fig_num;
        CREATE TABLE stg.fid AS SELECT fig_num || '' AS fig_num, fig_id FROM figs;
        CREATE INDEX stg.fid_i ON fid(fig_num);
        """)
        progress("Writing set contents …", 0.93)
        try db.exec("""
        INSERT INTO set_parts(set_id, part_id, color_id, quantity, is_spare, fig_id)
            SELECT sid.set_id, pid.part_id, sp.color_id, sp.quantity, sp.is_spare, fid.fig_id
            FROM stg.sp sp JOIN stg.sid sid ON sid.set_num = sp.set_num
            JOIN stg.pid pid ON pid.part_num = sp.part_num
            LEFT JOIN stg.fid fid ON fid.fig_num = sp.fig_num
            ORDER BY sid.set_id, pid.part_id, sp.color_id;
        INSERT INTO part_color_img(part_id, color_id, img_url)
            SELECT pid.part_id, sp.color_id, MIN(sp.img)
            FROM stg.sp sp JOIN stg.pid pid ON pid.part_num = sp.part_num
            WHERE sp.img <> '' AND sp.color_id IS NOT NULL GROUP BY pid.part_id, sp.color_id;
        """)
        progress("Indexing …", 0.97)
        try db.exec(Schema.indexes)

        // ---- info ---------------------------------------------------------------------------
        let nSets = try db.scalarInt("SELECT COUNT(*) FROM sets")
        let nParts = try db.scalarInt("SELECT COUNT(*) FROM parts")
        let nRows = try db.scalarInt("SELECT COUNT(*) FROM set_parts")
        let fmt = DateFormatter(); fmt.dateFormat = "yyyy-MM-dd HH:mm"; fmt.locale = Locale(identifier: "en_US_POSIX")
        let info: [(String, String)] = [("built", fmt.string(from: Date())), ("source", sourceLabel),
            ("n_sets", "\(nSets)"), ("n_parts", "\(nParts)"), ("n_rows", "\(nRows)"),
            ("minifig_parts", haveMinifigs ? "1" : "0")]
        let insI = try db.prepare("INSERT INTO build_info VALUES (?,?)")
        for (k, v) in info { try insI.run([.text(k), .text(v)]) }
        try db.exec("ANALYZE")
        try db.exec("DETACH DATABASE stg")
        progress("Done", 1.0)
    }
}
