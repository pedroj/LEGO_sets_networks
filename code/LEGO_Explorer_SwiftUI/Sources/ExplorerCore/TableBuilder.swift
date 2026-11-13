import Foundation
import SQLite3

/// Rebuilds the explorer database from Rebrickable's public CSV downloads (no API key).
/// Port of `build_data.R` that works on iPadOS/macOS: download -> gunzip -> stage in SQLite ->
/// transform with SQL -> classify parts (PartClassifier) -> write the final tables.
/// The output schema is identical to tools/rds_to_sqlite.R.
public final class TableBuilder {
    public typealias Progress = (String, Double) -> Void

    public static let tables = ["themes", "colors", "part_categories", "parts", "sets", "inventories", "inventory_parts"]
    public static let optionalTables = ["inventory_minifigs"]
    static let urlTemplate = "https://cdn.rebrickable.com/media/downloads/%@.csv.gz"

    public init() {}

    // MARK: download

    /// Downloads every table into `dir` (created if needed). Needs no API key.
    public func download(to dir: URL, progress: @escaping Progress) async throws {
        try FileManager.default.createDirectory(at: dir, withIntermediateDirectories: true)
        let all = Self.tables + Self.optionalTables
        for (i, t) in all.enumerated() {
            progress("Downloading \(t) …", Double(i) / Double(all.count) * 0.4)
            let url = URL(string: String(format: Self.urlTemplate, t))!
            do {
                let (tmp, resp) = try await URLSession.shared.download(from: url)
                if let http = resp as? HTTPURLResponse, http.statusCode != 200 {
                    throw SQLiteError(message: "HTTP \(http.statusCode)")
                }
                let dest = dir.appendingPathComponent("\(t).csv.gz")
                try? FileManager.default.removeItem(at: dest)
                try FileManager.default.moveItem(at: tmp, to: dest)
            } catch {
                if Self.tables.contains(t) { throw SQLiteError(message: "Could not download \(t) from Rebrickable: \(error.localizedDescription)") }
            }
        }
    }

    // MARK: build

    static func findTable(_ dir: URL, _ t: String) -> URL? {
        for ext in ["csv.gz", "csv"] {
            let u = dir.appendingPathComponent("\(t).\(ext)")
            if FileManager.default.fileExists(atPath: u.path) { return u }
        }
        return nil
    }

    /// Builds `output` (replaced if present) from the CSVs in `csvDir`.
    public func build(csvDir: URL, output: URL, source: String, progress: Progress) throws {
        for t in Self.tables where Self.findTable(csvDir, t) == nil {
            throw SQLiteError(message: "Missing table \(t) in \(csvDir.path)")
        }
        try? FileManager.default.removeItem(at: output)
        let db = try SQLiteDB(path: output.path, readOnly: false)
        defer { db.close() }
        try db.exec("PRAGMA journal_mode = OFF; PRAGMA synchronous = OFF; PRAGMA temp_store = FILE;")

        // ---- 1. stage the raw CSV tables ------------------------------------------------------
        let staging: [(String, [(String, String)])] = [
            ("themes", [("id", "INTEGER"), ("name", "TEXT"), ("parent_id", "INTEGER")]),
            ("colors", [("id", "INTEGER"), ("name", "TEXT"), ("rgb", "TEXT"), ("is_trans", "TEXT")]),
            ("part_categories", [("id", "INTEGER"), ("name", "TEXT")]),
            ("parts", [("part_num", "TEXT"), ("name", "TEXT"), ("part_cat_id", "INTEGER"), ("part_material", "TEXT")]),
            ("sets", [("set_num", "TEXT"), ("name", "TEXT"), ("year", "INTEGER"), ("theme_id", "INTEGER"),
                      ("num_parts", "INTEGER"), ("img_url", "TEXT")]),
            ("inventories", [("id", "INTEGER"), ("version", "INTEGER"), ("set_num", "TEXT")]),
            ("inventory_parts", [("inventory_id", "INTEGER"), ("part_num", "TEXT"), ("color_id", "INTEGER"),
                                 ("quantity", "INTEGER"), ("is_spare", "TEXT"), ("img_url", "TEXT")]),
            ("inventory_minifigs", [("inventory_id", "INTEGER"), ("fig_num", "TEXT"), ("quantity", "INTEGER")]),
        ]
        var hasMinifigs = false
        for (k, (name, cols)) in staging.enumerated() {
            guard let file = Self.findTable(csvDir, name) else { continue }
            if name == "inventory_minifigs" { hasMinifigs = true }
            progress("Reading \(name) …", 0.4 + 0.3 * Double(k) / Double(staging.count))
            try db.exec("CREATE TABLE raw_\(name) (" + cols.map { "\($0.0) \($0.1)" }.joined(separator: ", ") + ")")
            try importCSV(db: db, file: file, table: "raw_\(name)", columns: cols.map(\.0))
        }

        // ---- 2. schema ------------------------------------------------------------------------
        progress("Building tables …", 0.72)
        try db.exec("""
            CREATE TABLE meta (key TEXT PRIMARY KEY, value TEXT);
            CREATE TABLE sets (id INTEGER PRIMARY KEY, set_num TEXT NOT NULL, set_name TEXT, year INTEGER, theme_id INTEGER, num_parts INTEGER, img_url TEXT, theme_name TEXT, theme_group TEXT);
            CREATE TABLE parts (id INTEGER PRIMARY KEY, part_num TEXT NOT NULL, part_name TEXT, category TEXT, subcategory TEXT, part_category TEXT, part_material TEXT, img_url TEXT, n_sets INTEGER, n_colors INTEGER, total_qty INTEGER);
            CREATE TABLE colors (color_id INTEGER PRIMARY KEY, name TEXT, rgb TEXT, is_trans INTEGER);
            CREATE TABLE set_parts (set_id INTEGER NOT NULL, part_id INTEGER NOT NULL, color_id INTEGER NOT NULL, quantity INTEGER, is_spare INTEGER, minifig TEXT);
            CREATE TABLE part_images (part_id INTEGER NOT NULL, color_id INTEGER NOT NULL, url TEXT, PRIMARY KEY (part_id, color_id)) WITHOUT ROWID;
            """)

        let pfx = RebrickableImage.prefix
        func strip(_ c: String) -> String { "CASE WHEN substr(\(c),1,\(pfx.count)) = '\(pfx)' THEN substr(\(c),\(pfx.count + 1)) ELSE \(c) END" }
        func isTrue(_ c: String) -> String { "CASE WHEN upper(substr(\(c),1,1)) IN ('T','1') THEN 1 ELSE 0 END" }

        try db.exec("BEGIN")
        // theme -> top-level theme ("theme group")
        try db.exec("""
            CREATE TABLE theme_root AS
            WITH RECURSIVE r(id, root) AS (
                SELECT id, id FROM raw_themes WHERE parent_id IS NULL OR parent_id NOT IN (SELECT id FROM raw_themes)
                UNION ALL SELECT t.id, r.root FROM raw_themes t JOIN r ON t.parent_id = r.id)
            SELECT r.id AS id, th.name AS group_name FROM r JOIN raw_themes th ON th.id = r.root;
            CREATE INDEX theme_root_id ON theme_root(id);
            """)
        // sets, newest first
        try db.exec("""
            INSERT INTO sets (set_num, set_name, year, theme_id, num_parts, img_url, theme_name, theme_group)
            SELECT s.set_num, s.name, s.year, s.theme_id, s.num_parts, \(strip("s.img_url")),
                   t.name, COALESCE(tr.group_name, t.name)
            FROM raw_sets s LEFT JOIN raw_themes t ON t.id = s.theme_id LEFT JOIN theme_root tr ON tr.id = s.theme_id
            ORDER BY s.year DESC, s.set_num;
            CREATE INDEX sets_num ON sets(set_num);
            """)
        try db.exec("""
            INSERT INTO colors (color_id, name, rgb, is_trans)
            SELECT id, name, rgb, \(isTrue("is_trans")) FROM raw_colors;
            """)

        // latest inventory version of every set / minifigure
        try db.exec("""
            CREATE INDEX raw_inv_set ON raw_inventories(set_num);
            CREATE TABLE inv_latest AS
            SELECT i.set_num AS set_num, i.id AS inv_id FROM raw_inventories i
            WHERE i.version = (SELECT MAX(j.version) FROM raw_inventories j WHERE j.set_num = i.set_num);
            CREATE INDEX inv_latest_set ON inv_latest(set_num);
            CREATE INDEX inv_latest_id ON inv_latest(inv_id);
            CREATE INDEX raw_ip_inv ON raw_inventory_parts(inventory_id);
            """)
        progress("Joining set inventories …", 0.78)
        try db.exec("""
            CREATE TABLE sp_tmp (set_id INTEGER, part_num TEXT, color_id INTEGER, quantity INTEGER, is_spare INTEGER, minifig TEXT, url TEXT);
            INSERT INTO sp_tmp
            SELECT s.id, ip.part_num, ip.color_id, ip.quantity, \(isTrue("ip.is_spare")), NULL, ip.img_url
            FROM inv_latest il JOIN sets s ON s.set_num = il.set_num
            JOIN raw_inventory_parts ip ON ip.inventory_id = il.inv_id;
            """)
        if hasMinifigs {
            // parts of the minifigures that come with each set
            try db.exec("""
                INSERT INTO sp_tmp
                SELECT s.id, ip.part_num, ip.color_id, ip.quantity * im.quantity, \(isTrue("ip.is_spare")), im.fig_num, ip.img_url
                FROM inv_latest il JOIN sets s ON s.set_num = il.set_num
                JOIN raw_inventory_minifigs im ON im.inventory_id = il.inv_id
                JOIN inv_latest fl ON fl.set_num = im.fig_num
                JOIN raw_inventory_parts ip ON ip.inventory_id = fl.inv_id;
                """)
        }
        try db.exec("COMMIT")

        // ---- 3. classify parts (same rules as classify_part.R) --------------------------------
        progress("Classifying parts …", 0.84)
        try db.exec("BEGIN")
        try db.exec("CREATE TABLE parts_tmp (part_num TEXT, part_name TEXT, category TEXT, subcategory TEXT, part_category TEXT, part_material TEXT)")
        let ins = try db.prepare("INSERT INTO parts_tmp VALUES (?,?,?,?,?,?)")
        let cls = PartClassifier()
        let sel = try db.prepare("""
            SELECT p.part_num, p.name, pc.name, p.part_material FROM raw_parts p
            LEFT JOIN raw_part_categories pc ON pc.id = p.part_cat_id
            """)
        while sqlite3_step(sel) == SQLITE_ROW {
            func t(_ i: Int32) -> String? { sqlite3_column_text(sel, i).map { String(cString: $0) } }
            let num = t(0) ?? "", name = t(1), rb = t(2), mat = t(3)
            let r = cls.classify(name: name, category: rb)
            db.bind(ins, [.text(num), name.map { .text($0) } ?? .null, .text(r.category), .text(r.subcategory),
                          rb.map { .text($0) } ?? .null, mat.map { .text($0) } ?? .null])
            sqlite3_step(ins); sqlite3_reset(ins)
        }
        sqlite3_finalize(sel); sqlite3_finalize(ins)
        try db.exec("COMMIT")

        // ---- 4. final tables -------------------------------------------------------------------
        progress("Writing final tables …", 0.9)
        try db.exec("BEGIN")
        try db.exec("""
            CREATE TABLE usage AS
            SELECT part_num, COUNT(DISTINCT set_id) AS n_sets, COUNT(DISTINCT color_id) AS n_colors, SUM(quantity) AS total_qty
            FROM sp_tmp GROUP BY part_num;
            CREATE INDEX usage_pn ON usage(part_num);
            CREATE TABLE part_img AS
            SELECT part_num, url FROM (
                SELECT part_num, url, ROW_NUMBER() OVER (PARTITION BY part_num ORDER BY COUNT(*) DESC, url) AS rn
                FROM sp_tmp WHERE url IS NOT NULL AND url <> '' GROUP BY part_num, url) WHERE rn = 1;
            CREATE INDEX part_img_pn ON part_img(part_num);
            CREATE INDEX parts_tmp_pn ON parts_tmp(part_num);
            INSERT INTO parts (part_num, part_name, category, subcategory, part_category, part_material, img_url, n_sets, n_colors, total_qty)
            SELECT pt.part_num, pt.part_name, pt.category, pt.subcategory, pt.part_category, pt.part_material,
                   COALESCE(\(strip("pi.url")), ''), COALESCE(u.n_sets, 0), COALESCE(u.n_colors, 0), COALESCE(u.total_qty, 0)
            FROM parts_tmp pt LEFT JOIN usage u ON u.part_num = pt.part_num LEFT JOIN part_img pi ON pi.part_num = pt.part_num
            ORDER BY COALESCE(u.n_sets, 0) DESC, pt.part_num;
            CREATE INDEX parts_num ON parts(part_num);
            INSERT INTO set_parts (set_id, part_id, color_id, quantity, is_spare, minifig)
            SELECT t.set_id, p.id, t.color_id, t.quantity, t.is_spare, t.minifig
            FROM sp_tmp t JOIN parts p ON p.part_num = t.part_num
            ORDER BY t.set_id, p.id, t.color_id;
            INSERT INTO part_images (part_id, color_id, url)
            SELECT p.id, t.color_id, \(strip("MIN(t.url)"))
            FROM sp_tmp t JOIN parts p ON p.part_num = t.part_num
            WHERE t.url IS NOT NULL AND t.url <> '' GROUP BY p.id, t.color_id;
            """)
        let nSets = try db.scalarInt("SELECT COUNT(*) FROM sets")
        let nParts = try db.scalarInt("SELECT COUNT(*) FROM parts")
        let nRows = try db.scalarInt("SELECT COUNT(*) FROM set_parts")
        let df = DateFormatter(); df.dateFormat = "yyyy-MM-dd HH:mm"; df.locale = Locale(identifier: "en_US_POSIX")
        let meta: [(String, String)] = [("built", df.string(from: Date())), ("source", source),
            ("n_sets", "\(nSets)"), ("n_parts", "\(nParts)"), ("n_rows", "\(nRows)"),
            ("minifig_parts", hasMinifigs ? "1" : "0"), ("image_prefix", pfx), ("schema_version", "1")]
        for (k, v) in meta { try db.exec("INSERT INTO meta VALUES ('\(k)', '\(v.replacingOccurrences(of: "'", with: "''"))')") }
        try db.exec("COMMIT")

        // ---- 5. tidy up ------------------------------------------------------------------------
        progress("Indexing and compacting …", 0.95)
        try db.exec("""
            DROP TABLE sp_tmp; DROP TABLE parts_tmp; DROP TABLE usage; DROP TABLE part_img;
            DROP TABLE theme_root; DROP TABLE inv_latest;
            DROP TABLE raw_themes; DROP TABLE raw_colors; DROP TABLE raw_part_categories; DROP TABLE raw_parts;
            DROP TABLE raw_sets; DROP TABLE raw_inventories; DROP TABLE raw_inventory_parts;
            """)
        if hasMinifigs { try db.exec("DROP TABLE raw_inventory_minifigs") }
        try db.exec("""
            CREATE INDEX idx_sp_set ON set_parts(set_id);
            CREATE INDEX idx_sp_part ON set_parts(part_id);
            """)
        try db.exec("DROP INDEX IF EXISTS sets_num; DROP INDEX IF EXISTS parts_num; CREATE INDEX idx_sets_num ON sets(set_num); CREATE INDEX idx_parts_num ON parts(part_num);")
        try db.exec("ANALYZE")
        try db.exec("VACUUM")
        progress("Done", 1.0)
    }

    /// Streams a (gzipped) CSV into `table`; empty fields become NULL. Columns are matched by header name.
    private func importCSV(db: SQLiteDB, file: URL, table: String, columns: [String]) throws {
        let stmt = try db.prepare("INSERT INTO \(table) VALUES (" + Array(repeating: "?", count: columns.count).joined(separator: ",") + ")")
        defer { sqlite3_finalize(stmt) }
        try db.exec("BEGIN")
        var map: [Int?] = []          // table column -> CSV column index
        var header = true
        let reader = CSVReader { row in
            if header {
                header = false
                map = columns.map { c in row.firstIndex(of: c) }
                return
            }
            for (i, idx) in map.enumerated() {
                if let idx, idx < row.count, !row[idx].isEmpty {
                    sqlite3_bind_text(stmt, Int32(i + 1), row[idx], -1, unsafeBitCast(-1, to: sqlite3_destructor_type.self))
                } else {
                    sqlite3_bind_null(stmt, Int32(i + 1))
                }
            }
            sqlite3_step(stmt); sqlite3_reset(stmt)
        }
        if file.pathExtension == "gz" {
            try Gzip.stream(file: file) { try reader.feed($0) }
        } else {
            let fh = try FileHandle(forReadingFrom: file)
            defer { try? fh.close() }
            while let d = try fh.read(upToCount: 1 << 18), !d.isEmpty {
                try d.withUnsafeBytes { try reader.feed($0.bindMemory(to: UInt8.self)) }
            }
        }
        try reader.finish()
        try db.exec("COMMIT")
    }
}

// MARK: - where the database lives

public enum DataLocation {
    public static let appFolderName = "LEGO Sets & Parts Explorer"

    /// Writable copy (survives app updates); created from the bundled database on first launch.
    public static var userDirectory: URL {
        let base = FileManager.default.urls(for: .applicationSupportDirectory, in: .userDomainMask)[0]
        return base.appendingPathComponent(appFolderName, isDirectory: true)
    }
    public static var userDatabase: URL { userDirectory.appendingPathComponent("lego_explorer.sqlite") }

    /// The database that ships inside the app (or LEGO_DB, for development).
    public static var bundledDatabase: URL? {
        if let p = ProcessInfo.processInfo.environment["LEGO_DB"] { return URL(fileURLWithPath: p) }
        return Bundle.main.url(forResource: "lego_explorer", withExtension: "sqlite")
    }

    /// Path of the database to open; installs the bundled one when there is no user copy yet.
    public static func resolve() throws -> URL {
        let fm = FileManager.default
        if let p = ProcessInfo.processInfo.environment["LEGO_DB"] { return URL(fileURLWithPath: p) }
        if fm.fileExists(atPath: userDatabase.path), (try? ExplorerDatabase(path: userDatabase.path)) != nil {
            return userDatabase
        }
        guard let bundled = bundledDatabase else { throw SQLiteError(message: "No bundled database found.") }
        try restoreBundled(from: bundled)
        return userDatabase
    }

    public static func restoreBundled(from bundled: URL? = nil) throws {
        guard let src = bundled ?? bundledDatabase else { throw SQLiteError(message: "No bundled database found.") }
        let fm = FileManager.default
        try fm.createDirectory(at: userDirectory, withIntermediateDirectories: true)
        try? fm.removeItem(at: userDatabase)
        try fm.copyItem(at: src, to: userDatabase)
    }
}
