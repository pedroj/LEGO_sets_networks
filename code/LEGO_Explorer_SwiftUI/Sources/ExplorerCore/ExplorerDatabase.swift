import Foundation

/// Read-only access to the explorer SQLite database (schema: see tools/rds_to_sqlite.R).
public final class ExplorerDatabase {
    public let db: SQLiteDB
    public let path: String

    public init(path: String) throws {
        self.path = path
        db = try SQLiteDB(path: path, readOnly: true)
        // quick sanity check that this is an explorer database
        _ = try db.scalarInt("SELECT COUNT(*) FROM sets LIMIT 1")
    }

    public func close() { db.close() }

    public func info() -> DataInfo {
        var d = DataInfo()
        try? db.query("SELECT key, value FROM meta") { r in
            let v = r.text(1)
            switch r.text(0) {
            case "built": d.built = v
            case "source": d.source = v
            case "n_sets": d.nSets = Int(v) ?? 0
            case "n_parts": d.nParts = Int(v) ?? 0
            case "n_rows": d.nRows = Int(v) ?? 0
            case "minifig_parts": d.minifigParts = (v == "1")
            default: break
            }
        }
        return d
    }

    // MARK: whole tables (small enough to keep in memory: ~28k sets, ~65k parts)

    public func loadSets() -> [SetRecord] {
        var out: [SetRecord] = []
        out.reserveCapacity(30_000)
        try? db.query("SELECT id, set_num, set_name, year, num_parts, img_url, theme_name, theme_group FROM sets ORDER BY id") { r in
            out.append(SetRecord(id: r.int(0), setNum: r.text(1), name: r.text(2), year: r.optInt(3),
                                 numParts: r.int(4), image: r.optText(5), themeName: r.optText(6), themeGroup: r.optText(7)))
        }
        return out
    }

    public func loadParts() -> [PartRecord] {
        var out: [PartRecord] = []
        out.reserveCapacity(70_000)
        try? db.query("SELECT id, part_num, part_name, category, subcategory, part_category, img_url, n_sets, n_colors, total_qty FROM parts ORDER BY id") { r in
            out.append(PartRecord(id: r.int(0), partNum: r.text(1), name: r.text(2), category: r.text(3),
                                  subcategory: r.text(4), rbCategory: r.optText(5), image: r.optText(6),
                                  nSets: r.int(7), nColors: r.int(8), totalQty: r.int(9)))
        }
        return out
    }

    // MARK: detail queries (index-driven)

    /// The parts list of one set. Spare and minifigure rows can be left out; colours can be merged.
    public func setLines(setID: Int, includeSpares: Bool, includeMinifigParts: Bool, mergeColors: Bool) -> [PartLine] {
        var sql = """
            SELECT sp.part_id, p.part_num, p.part_name, p.part_category, p.category, p.subcategory,
                   c.name, c.rgb, sp.quantity, sp.is_spare, sp.minifig, pi.url
            FROM set_parts sp
            JOIN parts p ON p.id = sp.part_id
            LEFT JOIN colors c ON c.color_id = sp.color_id
            LEFT JOIN part_images pi ON pi.part_id = sp.part_id AND pi.color_id = sp.color_id
            WHERE sp.set_id = ?
            """
        if !includeSpares { sql += " AND sp.is_spare = 0" }
        if !includeMinifigParts { sql += " AND sp.minifig IS NULL" }
        sql += " ORDER BY p.category, p.subcategory, p.part_num, sp.color_id, sp.rowid"

        var lines: [PartLine] = []
        try? db.query(sql, [.int(setID)]) { r in
            lines.append(PartLine(id: lines.count, partID: r.int(0), partNum: r.text(1), partName: r.text(2),
                                  rbCategory: r.text(3), category: r.text(4), subcategory: r.text(5),
                                  colorName: r.text(6), rgb: r.optText(7) ?? "FFFFFF", quantity: r.int(8),
                                  isSpare: r.int(9) != 0, minifig: r.optText(10) ?? "",
                                  imageURL: RebrickableImage.url(r.optText(11))))
        }
        return mergeColors ? ExplorerDatabase.mergeColours(lines) : lines
    }

    /// Merge colours of the same part (R: group by part, quantity summed, colours listed).
    public static func mergeColours(_ lines: [PartLine]) -> [PartLine] {
        var order: [Int] = []
        var groups: [Int: [PartLine]] = [:]
        for l in lines {
            if groups[l.partID] == nil { order.append(l.partID) }
            groups[l.partID, default: []].append(l)
        }
        var merged: [PartLine] = []
        for pid in order {
            let g = groups[pid]!
            var seen = Set<String>(), names: [String] = []
            for l in g where seen.insert(l.colorName).inserted { names.append(l.colorName) }
            let f = g[0]
            merged.append(PartLine(id: merged.count, partID: pid, partNum: f.partNum, partName: f.partName,
                                   rbCategory: f.rbCategory, category: f.category, subcategory: f.subcategory,
                                   colorName: names.joined(separator: ", "), rgb: f.rgb,
                                   quantity: g.reduce(0) { $0 + $1.quantity }, isSpare: false, minifig: "",
                                   imageURL: g.compactMap(\.imageURL).first))
        }
        return merged
    }

    public func colorLines(partID: Int) -> [ColorLine] {
        var out: [ColorLine] = []
        try? db.query("""
            SELECT sp.color_id, c.name, c.rgb, COUNT(DISTINCT sp.set_id) AS n_sets, SUM(sp.quantity), pi.url
            FROM set_parts sp
            LEFT JOIN colors c ON c.color_id = sp.color_id
            LEFT JOIN part_images pi ON pi.part_id = sp.part_id AND pi.color_id = sp.color_id
            WHERE sp.part_id = ?
            GROUP BY sp.color_id
            ORDER BY n_sets DESC, sp.color_id
            """, [.int(partID)]) { r in
            out.append(ColorLine(colorID: r.int(0), name: r.optText(1) ?? "?", rgb: r.optText(2) ?? "FFFFFF",
                                 sets: r.int(3), pieces: r.int(4), imageURL: RebrickableImage.url(r.optText(5))))
        }
        return out
    }

    public func setLines(partID: Int) -> [PartSetLine] {
        var out: [PartSetLine] = []
        try? db.query("""
            SELECT s.id, s.set_num, s.set_name, s.year, s.theme_name, SUM(sp.quantity), s.img_url,
                   GROUP_CONCAT(DISTINCT c.name)
            FROM set_parts sp
            JOIN sets s ON s.id = sp.set_id
            LEFT JOIN colors c ON c.color_id = sp.color_id
            WHERE sp.part_id = ?
            GROUP BY s.id
            ORDER BY s.id
            """, [.int(partID)]) { r in
            out.append(PartSetLine(setID: r.int(0), setNum: r.text(1), setName: r.text(2), year: r.optInt(3),
                                   themeName: r.optText(4) ?? "", quantity: r.int(5),
                                   colours: (r.optText(7) ?? "").replacingOccurrences(of: ",", with: ", "),
                                   imageURL: RebrickableImage.url(r.optText(6))))
        }
        return out
    }

    /// Totals for a set's current (filtered) list, shown in the detail header.
    public func partsTotals(_ lines: [PartLine]) -> (types: Int, colours: Int, pieces: Int) {
        (Set(lines.map(\.partID)).count, Set(lines.map(\.colorName)).count, lines.reduce(0) { $0 + $1.quantity })
    }
}
