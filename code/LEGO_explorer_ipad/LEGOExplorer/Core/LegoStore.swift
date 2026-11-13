import Foundation

// MARK: - Value types

struct SetFilter: Hashable {
    var setNum = ""
    var themeGroup = "All"
    var theme = "All"
    var name = ""
    var yearFrom = 1949
    var yearTo = 2030
}

struct PartFilter: Hashable {
    var partNum = ""
    var name = ""
    var category = "All"
    var subcategory = "All"
    var rbCategory = "All"
    var onlyInSets = true
}

struct SetListOptions: Hashable {
    var mergeColors = false
    var showSpares = false
    var showMinifigParts = true
}

struct SetRow: Identifiable, Hashable {
    let id: Int
    let setNum: String
    let name: String
    let year: Int?
    let themeName: String
    let themeGroup: String
    let numParts: Int
    let imgURL: URL?
}

struct PartRow: Identifiable, Hashable {
    let id: Int
    let partNum: String
    let name: String
    let category: String
    let subcategory: String
    let rbCategory: String
    let material: String
    let nSets: Int
    let nColors: Int
    let totalQty: Int
    let imgURL: URL?
}

/// One line of a set's parts list (or, merged, one part with all its colours).
struct PartLine: Identifiable, Hashable {
    let id: Int
    let partID: Int
    let partNum: String
    let name: String
    let category: String
    let subcategory: String
    let rbCategory: String
    let colorName: String        // merged mode: "Red, Blue"
    let rgb: String?             // nil when merged
    let quantity: Int
    let isSpare: Bool
    let minifig: String?
    let imgURL: URL?
}

struct SetContents {
    let lines: [PartLine]
    var partTypes: Int { Set(lines.map(\.partID)).count }
    var colours: Int { Set(lines.map(\.colorName)).count }
    var pieces: Int { lines.reduce(0) { $0 + $1.quantity } }
}

struct ColorUse: Identifiable, Hashable {
    let id: Int
    let name: String
    let rgb: String?
    let sets: Int
    let pieces: Int
    let imgURL: URL?
}

struct PartSetUse: Identifiable, Hashable {
    let id: Int            // set_id
    let set: SetRow
    let quantity: Int
    let colours: String
}

struct PartDetail {
    let part: PartRow
    let colors: [ColorUse]
    let sets: [PartSetUse]
    var totalPieces: Int { colors.reduce(0) { $0 + $1.pieces } }
}

struct BuildInfo: Equatable {
    var built = ""
    var source = ""
    var nSets = 0
    var nParts = 0
    var nRows = 0
    var minifigParts = false
}

struct Lookups {
    var themeGroups: [String] = []
    var themesByGroup: [String: [String]] = [:]
    var allThemes: [String] = []
    var categories: [String] = []
    var subcategoriesByCategory: [String: [String]] = [:]
    var allSubcategories: [String] = []
    var rbCategories: [String] = []
    var yearRange: ClosedRange<Int> = 1949...2026
    var info = BuildInfo()
}

// MARK: - Store

/// Read-only access to lego.sqlite. All searches run in SQL, so they are instant.
actor LegoStore {
    private let db: SQLiteDB

    init(path: String) throws {
        db = try SQLiteDB(path: path, readonly: true)
        // sanity check: right schema?
        _ = try db.scalarInt("SELECT COUNT(*) FROM sets LIMIT 1")
    }

    private static func like(_ s: String) -> String {
        s.replacingOccurrences(of: "\\", with: "\\\\")
            .replacingOccurrences(of: "%", with: "\\%")
            .replacingOccurrences(of: "_", with: "\\_")
    }
    private static func url(_ s: String?) -> URL? {
        guard let s, !s.isEmpty else { return nil }
        return URL(string: s)
    }

    func lookups() throws -> Lookups {
        var l = Lookups()
        let pairs = try db.query("SELECT DISTINCT theme_group, theme_name FROM sets WHERE theme_name IS NOT NULL ORDER BY 1, 2") {
            ($0.optText(0) ?? "", $0.text(1))
        }
        for (g, t) in pairs { l.themesByGroup[g, default: []].append(t) }
        l.themeGroups = l.themesByGroup.keys.filter { !$0.isEmpty }.sorted()
        l.allThemes = Array(Set(pairs.map(\.1))).sorted()
        let cs = try db.query("SELECT DISTINCT category, subcategory FROM parts ORDER BY 1, 2") { ($0.text(0), $0.text(1)) }
        for (c, s) in cs { l.subcategoriesByCategory[c, default: []].append(s) }
        l.categories = l.subcategoriesByCategory.keys.sorted()
        l.allSubcategories = Array(Set(cs.map(\.1))).sorted()
        l.rbCategories = try db.query("SELECT DISTINCT part_category FROM parts WHERE part_category IS NOT NULL ORDER BY 1") { $0.text(0) }
        let yr = try db.query("SELECT MIN(year), MAX(year) FROM sets") { ($0.optInt(0), $0.optInt(1)) }.first
        if let lo = yr?.0, let hi = yr?.1, lo <= hi { l.yearRange = lo...hi }
        let kv = Dictionary(uniqueKeysWithValues: try db.query("SELECT key, value FROM build_info") { ($0.text(0), $0.text(1)) })
        l.info = BuildInfo(built: kv["built"] ?? "", source: kv["source"] ?? "",
                           nSets: Int(kv["n_sets"] ?? "") ?? 0, nParts: Int(kv["n_parts"] ?? "") ?? 0,
                           nRows: Int(kv["n_rows"] ?? "") ?? 0, minifigParts: kv["minifig_parts"] == "1")
        return l
    }

    // MARK: Sets

    private static let setCols = "set_id, set_num, set_name, year, theme_name, theme_group, num_parts, set_img_url"
    private static func setRow(_ s: Stmt, at o: Int = 0) -> SetRow {
        SetRow(id: s.int(o), setNum: s.text(o + 1), name: s.text(o + 2), year: s.optInt(o + 3),
               themeName: s.text(o + 4), themeGroup: s.optText(o + 5) ?? s.text(o + 4),
               numParts: s.int(o + 6), imgURL: url(s.optText(o + 7)))
    }

    func searchSets(_ f: SetFilter) throws -> [SetRow] {
        var where_: [String] = []
        var args: [DBValue] = []
        let q = f.setNum.trimmingCharacters(in: .whitespaces)
        if !q.isEmpty {
            // "75144-1" exact; "75144" all its variants; otherwise partial
            let exactOrVariant = try db.scalarInt(
                "SELECT COUNT(*) FROM sets WHERE set_num = ?1 OR set_num LIKE ?2 ESCAPE '\\'",
                [.text(q), .text(Self.like(q) + "-%")])
            if exactOrVariant > 0 {
                where_.append("(set_num = ? OR set_num LIKE ? ESCAPE '\\')")
                args += [.text(q), .text(Self.like(q) + "-%")]
            } else {
                where_.append("set_num LIKE ? ESCAPE '\\'")
                args.append(.text("%" + Self.like(q) + "%"))
            }
        }
        if f.themeGroup != "All" { where_.append("theme_group = ?"); args.append(.text(f.themeGroup)) }
        if f.theme != "All" { where_.append("theme_name = ?"); args.append(.text(f.theme)) }
        let n = f.name.trimmingCharacters(in: .whitespaces)
        if !n.isEmpty { where_.append("set_name LIKE ? ESCAPE '\\'"); args.append(.text("%" + Self.like(n) + "%")) }
        where_.append("(year IS NULL OR (year >= ? AND year <= ?))")
        args += [.int(f.yearFrom), .int(f.yearTo)]
        let sql = "SELECT \(Self.setCols) FROM sets WHERE \(where_.joined(separator: " AND ")) ORDER BY year DESC, set_num COLLATE BINARY"
        return try db.query(sql, args) { Self.setRow($0) }
    }

    func set(id: Int) throws -> SetRow? {
        try db.query("SELECT \(Self.setCols) FROM sets WHERE set_id = ?", [.int(id)]) { Self.setRow($0) }.first
    }
    func set(number: String) throws -> SetRow? {
        try db.query("SELECT \(Self.setCols) FROM sets WHERE set_num = ? LIMIT 1", [.text(number)]) { Self.setRow($0) }.first
    }

    func contents(setID: Int, options o: SetListOptions) throws -> SetContents {
        var sql = """
        SELECT p.part_id, p.part_num, p.part_name, p.category, p.subcategory, p.part_category,
               sp.color_id, c.color_name, c.rgb, sp.quantity, sp.is_spare, f.fig_num,
               COALESCE(i.img_url, p.part_img_url)
        FROM set_parts sp JOIN parts p ON p.part_id = sp.part_id
        LEFT JOIN colors c ON c.color_id = sp.color_id
        LEFT JOIN figs f ON f.fig_id = sp.fig_id
        LEFT JOIN part_color_img i ON i.part_id = sp.part_id AND i.color_id = sp.color_id
        WHERE sp.set_id = ?
        """
        if !o.showSpares { sql += " AND sp.is_spare = 0" }
        if !o.showMinifigParts { sql += " AND sp.fig_id IS NULL" }
        sql += " ORDER BY p.category COLLATE BINARY, p.subcategory COLLATE BINARY, p.part_num COLLATE BINARY, sp.color_id"
        var n = 0
        let raw = try db.query(sql, [.int(setID)]) { s -> PartLine in
            n += 1
            return PartLine(id: n, partID: s.int(0), partNum: s.text(1), name: s.text(2), category: s.text(3),
                            subcategory: s.text(4), rbCategory: s.optText(5) ?? "", colorName: s.optText(7) ?? "?",
                            rgb: s.optText(8), quantity: s.int(9), isSpare: s.int(10) != 0,
                            minifig: s.optText(11), imgURL: Self.url(s.optText(12)))
        }
        guard o.mergeColors else { return SetContents(lines: raw) }
        // combine colours of the same part (rows are already ordered by part)
        var merged: [PartLine] = []
        var index: [Int: Int] = [:]
        var names: [Int: [String]] = [:]
        for l in raw {
            if let i = index[l.partID] {
                let m = merged[i]
                if !(names[l.partID] ?? []).contains(l.colorName) { names[l.partID, default: []].append(l.colorName) }
                merged[i] = PartLine(id: m.id, partID: m.partID, partNum: m.partNum, name: m.name, category: m.category,
                                     subcategory: m.subcategory, rbCategory: m.rbCategory,
                                     colorName: names[l.partID]!.joined(separator: ", "), rgb: nil,
                                     quantity: m.quantity + l.quantity, isSpare: false, minifig: nil,
                                     imgURL: m.imgURL ?? l.imgURL)
            } else {
                index[l.partID] = merged.count
                names[l.partID] = [l.colorName]
                merged.append(PartLine(id: merged.count + 1, partID: l.partID, partNum: l.partNum, name: l.name,
                                       category: l.category, subcategory: l.subcategory, rbCategory: l.rbCategory,
                                       colorName: l.colorName, rgb: nil, quantity: l.quantity, isSpare: false,
                                       minifig: nil, imgURL: l.imgURL))
            }
        }
        return SetContents(lines: merged)
    }

    // MARK: Parts

    private static let partCols = "part_id, part_num, part_name, category, subcategory, part_category, part_material, n_sets, n_colors, total_qty, part_img_url"
    private static func partRow(_ s: Stmt) -> PartRow {
        PartRow(id: s.int(0), partNum: s.text(1), name: s.text(2), category: s.text(3), subcategory: s.text(4),
                rbCategory: s.optText(5) ?? "", material: s.optText(6) ?? "", nSets: s.int(7), nColors: s.int(8),
                totalQty: s.int(9), imgURL: url(s.optText(10)))
    }

    func searchParts(_ f: PartFilter) throws -> [PartRow] {
        var base: [String] = []
        var baseArgs: [DBValue] = []
        if f.onlyInSets { base.append("n_sets > 0") }
        // Part-number matching: exact first, then prefix, then partial (within the scope above)
        var numClause: String?
        var numArgs: [DBValue] = []
        let q = f.partNum.trimmingCharacters(in: .whitespaces)
        if !q.isEmpty {
            let scope = base.isEmpty ? "1" : base.joined(separator: " AND ")
            let tiers: [(String, [DBValue])] = [
                ("part_num = ?", [.text(q)]),
                ("part_num LIKE ? ESCAPE '\\'", [.text(Self.like(q) + "%")]),
                ("part_num LIKE ? ESCAPE '\\'", [.text("%" + Self.like(q) + "%")]),
            ]
            for (c, a) in tiers {
                if try db.scalarInt("SELECT COUNT(*) FROM parts WHERE \(scope) AND \(c)", a) > 0 {
                    numClause = c; numArgs = a; break
                }
            }
            if numClause == nil { return [] }
        }
        var where_ = base
        var args = baseArgs
        if let c = numClause { where_.append(c); args += numArgs }
        baseArgs = []
        let n = f.name.trimmingCharacters(in: .whitespaces)
        if !n.isEmpty { where_.append("part_name LIKE ? ESCAPE '\\'"); args.append(.text("%" + Self.like(n) + "%")) }
        if f.category != "All" { where_.append("category = ?"); args.append(.text(f.category)) }
        if f.subcategory != "All" { where_.append("subcategory = ?"); args.append(.text(f.subcategory)) }
        if f.rbCategory != "All" { where_.append("part_category = ?"); args.append(.text(f.rbCategory)) }
        let w = where_.isEmpty ? "" : "WHERE " + where_.joined(separator: " AND ")
        // part_id order = most used first, then part number
        return try db.query("SELECT \(Self.partCols) FROM parts \(w) ORDER BY part_id", args) { Self.partRow($0) }
    }

    func part(id: Int) throws -> PartRow? {
        try db.query("SELECT \(Self.partCols) FROM parts WHERE part_id = ?", [.int(id)]) { Self.partRow($0) }.first
    }
    func part(number: String) throws -> PartRow? {
        try db.query("SELECT \(Self.partCols) FROM parts WHERE part_num = ? LIMIT 1", [.text(number)]) { Self.partRow($0) }.first
    }

    func detail(partID: Int) throws -> PartDetail? {
        guard let p = try part(id: partID) else { return nil }
        let colors = try db.query("""
            SELECT sp.color_id, COALESCE(c.color_name, '?'), c.rgb, COUNT(DISTINCT sp.set_id), SUM(sp.quantity), MIN(i.img_url)
            FROM set_parts sp LEFT JOIN colors c ON c.color_id = sp.color_id
            LEFT JOIN part_color_img i ON i.part_id = sp.part_id AND i.color_id = sp.color_id
            WHERE sp.part_id = ? GROUP BY sp.color_id ORDER BY 4 DESC, 2
            """, [.int(partID)]) {
            ColorUse(id: $0.int(0), name: $0.text(1), rgb: $0.optText(2), sets: $0.int(3), pieces: $0.int(4),
                     imgURL: Self.url($0.optText(5)))
        }
        // one row per set + colour; combine in Swift (names can contain commas)
        var order: [Int] = []
        var qty: [Int: Int] = [:]
        var cols: [Int: [String]] = [:]
        var rows: [Int: SetRow] = [:]
        _ = try db.query("""
            SELECT \(Self.setCols.split(separator: ",").map { "s." + $0.trimmingCharacters(in: .whitespaces) }.joined(separator: ", ")),
                   COALESCE(c.color_name, '?'), sp.quantity
            FROM set_parts sp JOIN sets s ON s.set_id = sp.set_id LEFT JOIN colors c ON c.color_id = sp.color_id
            WHERE sp.part_id = ? ORDER BY s.year DESC, s.set_num COLLATE BINARY, c.color_name
            """, [.int(partID)]) { s -> Int in
            let r = Self.setRow(s)
            if rows[r.id] == nil { rows[r.id] = r; order.append(r.id) }
            qty[r.id, default: 0] += s.int(9)
            let cn = s.text(8)
            if !(cols[r.id] ?? []).contains(cn) { cols[r.id, default: []].append(cn) }
            return 0
        }
        let sets = order.map { PartSetUse(id: $0, set: rows[$0]!, quantity: qty[$0] ?? 0, colours: (cols[$0] ?? []).joined(separator: ", ")) }
        return PartDetail(part: p, colors: colors, sets: sets)
    }
}
