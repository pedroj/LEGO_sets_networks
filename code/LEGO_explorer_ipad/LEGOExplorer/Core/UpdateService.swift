import Foundation

/// "Update tables": downloads Rebrickable's public CSV files (no API key) and rebuilds
/// the SQLite database on the device.
enum UpdateService {
    static func update(progress: @escaping @Sendable (String, Double) -> Void) async throws {
        let fm = FileManager.default
        let work = fm.temporaryDirectory.appendingPathComponent("rebrickable-\(UUID().uuidString)")
        try fm.createDirectory(at: work, withIntermediateDirectories: true)
        defer { try? fm.removeItem(at: work) }

        // 1. download
        let tables = DataBuilder.requiredTables + DataBuilder.optionalTables
        for (i, t) in tables.enumerated() {
            progress("Downloading \(t) …", 0.30 * Double(i) / Double(tables.count))
            do {
                let (tmp, resp) = try await URLSession.shared.download(from: DataBuilder.downloadURL(t))
                guard (resp as? HTTPURLResponse)?.statusCode == 200 else {
                    throw DBError(message: "Rebrickable answered HTTP \((resp as? HTTPURLResponse)?.statusCode ?? 0) for \(t)")
                }
                try fm.moveItem(at: tmp, to: work.appendingPathComponent("\(t).csv.gz"))
            } catch {
                if DataBuilder.optionalTables.contains(t) { continue }   // minifigure parts are optional
                throw DBError(message: "Could not download \(t): \(error.localizedDescription)")
            }
        }

        // 2. build the new database next to the current one
        let newDB = DataLocation.supportDir.appendingPathComponent("lego-new.sqlite")
        try await Task.detached(priority: .userInitiated) {
            try DataBuilder.build(sourceDir: work, output: newDB, sourceLabel: "cdn.rebrickable.com (downloaded on iPad)") {
                progress($0, $1)
            }
        }.value

        // 3. swap it in
        let dest = DataLocation.customDB
        if fm.fileExists(atPath: dest.path) { try fm.removeItem(at: dest) }
        try fm.moveItem(at: newDB, to: dest)
        var v = URLResourceValues(); v.isExcludedFromBackup = true
        var d = dest; try? d.setResourceValues(v)
    }
}
