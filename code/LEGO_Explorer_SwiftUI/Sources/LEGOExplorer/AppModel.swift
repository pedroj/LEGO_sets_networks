import SwiftUI
import ExplorerCore

/// Fast substring test on UTF-8 bytes (cheaper than String.contains over ~90k records).
@inline(__always)
func containsBytes(_ hay: String, _ needle: [UInt8]) -> Bool {
    hay.utf8.withContiguousStorageIfAvailable { h in
        needle.withUnsafeBufferPointer { n in memmem(h.baseAddress, h.count, n.baseAddress, n.count) != nil }
    } ?? hay.contains(String(decoding: needle, as: UTF8.self))
}

@MainActor
final class AppModel: ObservableObject {
    enum Mode: String, CaseIterable, Identifiable { case sets = "Sets", parts = "Parts"; var id: String { rawValue } }
    enum SetSort: String, CaseIterable, Identifiable {
        case newest = "Newest first", oldest = "Oldest first", number = "Set number", name = "Name", parts = "Most parts"
        var id: String { rawValue }
    }
    enum PartSort: String, CaseIterable, Identifiable {
        case popular = "Most used", number = "Part number", name = "Name", colours = "Most colours"
        var id: String { rawValue }
    }

    static let all = "All"

    // MARK: data
    private(set) var database: ExplorerDatabase?
    @Published var info = DataInfo()
    @Published var loadError: String?
    private var allSets: [SetRecord] = []
    private var allParts: [PartRecord] = []
    @Published var themeGroups: [String] = []
    private var themesByGroup: [String: [String]] = [:]
    @Published var themeChoices: [String] = [AppModel.all]
    @Published var yearBounds: ClosedRange<Int> = 1949...2026
    @Published var categories: [String] = []
    private var subcategoriesByCategory: [String: [String]] = [:]
    @Published var subcategoryChoices: [String] = [AppModel.all]
    @Published var rbCategories: [String] = []

    // MARK: navigation
    @Published var mode: Mode = .sets

    // MARK: set filters
    @Published var setNumQuery = "" { didSet { refreshSets() } }
    @Published var setNameQuery = "" { didSet { refreshSets() } }
    @Published var themeGroup = AppModel.all {
        didSet {
            if oldValue != themeGroup { updateThemeChoices(); theme = AppModel.all }
            refreshSets()
        }
    }
    @Published var theme = AppModel.all { didSet { refreshSets() } }
    @Published var yearFrom = 1949 { didSet { refreshSets() } }
    @Published var yearTo = 2026 { didSet { refreshSets() } }
    @Published var setSort: SetSort = .newest { didSet { refreshSets() } }
    // parts list options (set detail)
    @Published var mergeColors = false { didSet { reloadSetLines() } }
    @Published var showSpares = false { didSet { reloadSetLines() } }
    @Published var showMinifigParts = true { didSet { reloadSetLines() } }

    @Published private(set) var sets: [SetRecord] = []
    @Published var selectedSetID: Int? { didSet { if oldValue != selectedSetID { reloadSetLines() } } }

    // MARK: part filters
    @Published var partNumQuery = "" { didSet { refreshParts() } }
    @Published var partNameQuery = "" { didSet { refreshParts() } }
    @Published var category = AppModel.all {
        didSet {
            if oldValue != category { updateSubcategoryChoices(); subcategory = AppModel.all }
            refreshParts()
        }
    }
    @Published var subcategory = AppModel.all { didSet { refreshParts() } }
    @Published var rbCategory = AppModel.all { didSet { refreshParts() } }
    @Published var onlyInSets = true { didSet { refreshParts() } }
    @Published var partSort: PartSort = .popular { didSet { refreshParts() } }

    @Published private(set) var parts: [PartRecord] = []
    @Published var selectedPartID: Int? { didSet { if oldValue != selectedPartID { reloadPartDetail() } } }

    // MARK: detail data
    @Published private(set) var setLines: [PartLine] = []
    @Published private(set) var setTotals = (types: 0, colours: 0, pieces: 0)
    @Published private(set) var colorLines: [ColorLine] = []
    @Published private(set) var partSetLines: [PartSetLine] = []

    // MARK: updating
    @Published var updating = false
    @Published var updateProgress = 0.0
    @Published var updateMessage = ""
    @Published var updateResult: (ok: Bool, text: String)?

    // MARK: loading

    func load() {
        do {
            let url = try DataLocation.resolve()
            try open(path: url.path)
        } catch {
            loadError = error.localizedDescription
        }
    }

    private func open(path: String) throws {
        database?.close()
        let db = try ExplorerDatabase(path: path)
        database = db
        info = db.info()
        allSets = db.loadSets()
        allParts = db.loadParts()
        loadError = nil

        var groups = Set<String>(), byGroup: [String: Set<String>] = [:]
        var minY = Int.max, maxY = Int.min
        for s in allSets {
            if !s.themeGroup.isEmpty { groups.insert(s.themeGroup); byGroup[s.themeGroup, default: []].insert(s.themeName) }
            if let y = s.year { minY = min(minY, y); maxY = max(maxY, y) }
        }
        themeGroups = groups.sorted()
        themesByGroup = byGroup.mapValues { $0.filter { !$0.isEmpty }.sorted() }
        if minY <= maxY { yearBounds = minY...maxY }

        var cats = Set<String>(), sub: [String: Set<String>] = [:], rb = Set<String>()
        for p in allParts {
            cats.insert(p.category); sub[p.category, default: []].insert(p.subcategory)
            if !p.rbCategory.isEmpty { rb.insert(p.rbCategory) }
        }
        categories = cats.sorted()
        subcategoriesByCategory = sub.mapValues { $0.sorted() }
        rbCategories = rb.sorted()

        resetSetFilters(); resetPartFilters()
        selectedSetID = nil; selectedPartID = nil
        setLines = []; colorLines = []; partSetLines = []
    }

    // MARK: filters

    private func updateThemeChoices() {
        let th = themeGroup == Self.all ? Set(themesByGroup.values.flatMap { $0 }).sorted() : (themesByGroup[themeGroup] ?? [])
        themeChoices = [Self.all] + th
    }

    private func updateSubcategoryChoices() {
        let sc = category == Self.all ? Set(subcategoriesByCategory.values.flatMap { $0 }).sorted() : (subcategoriesByCategory[category] ?? [])
        subcategoryChoices = [Self.all] + sc
    }

    func resetSetFilters() {
        setNumQuery = ""; setNameQuery = ""; themeGroup = Self.all; theme = Self.all
        yearFrom = yearBounds.lowerBound; yearTo = yearBounds.upperBound
        updateThemeChoices()
        refreshSets()
    }

    func resetPartFilters() {
        partNumQuery = ""; partNameQuery = ""; category = Self.all; subcategory = Self.all; rbCategory = Self.all
        updateSubcategoryChoices()
        refreshParts()
    }

    var setFiltersActive: Bool {
        !setNumQuery.isEmpty || !setNameQuery.isEmpty || themeGroup != Self.all || theme != Self.all
            || yearFrom != yearBounds.lowerBound || yearTo != yearBounds.upperBound
    }

    private func refreshSets() {
        let q = setNumQuery.trimmingCharacters(in: .whitespaces).lowercased()
        let nameQ = setNameQuery.trimmingCharacters(in: .whitespaces).lowercased()
        let qb = Array(q.utf8), nb = Array(nameQ.utf8)
        var d = allSets
        if !q.isEmpty {
            var hit = d.filter { $0.numLower == q || $0.numLower.hasPrefix(q + "-") }
            if hit.isEmpty { hit = d.filter { containsBytes($0.numLower, qb) } }
            d = hit
        }
        if themeGroup != Self.all { d = d.filter { $0.themeGroup == themeGroup } }
        if theme != Self.all { d = d.filter { $0.themeName == theme } }
        if !nameQ.isEmpty { d = d.filter { containsBytes($0.nameLower, nb) } }
        let lo = yearFrom, hi = yearTo
        d = d.filter { s in s.year.map { $0 >= lo && $0 <= hi } ?? true }
        switch setSort {
        case .newest: break                                   // database order: year desc, set number
        case .oldest:
            d.sort { (a: SetRecord, b: SetRecord) -> Bool in
                let ya = a.year ?? Int.max, yb = b.year ?? Int.max
                return ya != yb ? ya < yb : a.setNum < b.setNum
            }
        case .number: d.sort { $0.setNum.localizedStandardCompare($1.setNum) == .orderedAscending }
        case .name: d.sort { $0.name.localizedCaseInsensitiveCompare($1.name) == .orderedAscending }
        case .parts: d.sort { $0.numParts > $1.numParts }
        }
        sets = d

        // A set-number search that points to one set selects it straight away
        if d.count == 1 { selectedSetID = d[0].id }
        else if !q.isEmpty, let exact = d.first(where: { $0.numLower == q || $0.numLower == q + "-1" }) { selectedSetID = exact.id }
    }

    private func refreshParts() {
        let q = partNumQuery.trimmingCharacters(in: .whitespaces).lowercased()
        let nameQ = partNameQuery.trimmingCharacters(in: .whitespaces).lowercased()
        let qb = Array(q.utf8), nb = Array(nameQ.utf8)
        var d = allParts
        if onlyInSets { d = d.filter { $0.nSets > 0 } }
        if !q.isEmpty {                                       // exact first, then prefix, then partial
            var hit = d.filter { $0.numLower == q }
            if hit.isEmpty { hit = d.filter { $0.numLower.hasPrefix(q) } }
            if hit.isEmpty { hit = d.filter { containsBytes($0.numLower, qb) } }
            d = hit
        }
        if !nameQ.isEmpty { d = d.filter { containsBytes($0.nameLower, nb) } }
        if category != Self.all { d = d.filter { $0.category == category } }
        if subcategory != Self.all { d = d.filter { $0.subcategory == subcategory } }
        if rbCategory != Self.all { d = d.filter { $0.rbCategory == rbCategory } }
        switch partSort {
        case .popular: break                                  // database order: most sets first
        case .number: d.sort { $0.partNum.localizedStandardCompare($1.partNum) == .orderedAscending }
        case .name: d.sort { $0.name.localizedCaseInsensitiveCompare($1.name) == .orderedAscending }
        case .colours: d.sort { $0.nColors > $1.nColors }
        }
        parts = d

        if d.count == 1 { selectedPartID = d[0].id }
        else if !q.isEmpty, let exact = d.first(where: { $0.numLower == q }) { selectedPartID = exact.id }
    }

    // MARK: details

    var selectedSet: SetRecord? { selectedSetID.flatMap { id in sets.first { $0.id == id } ?? allSets.first { $0.id == id } } }
    var selectedPart: PartRecord? { selectedPartID.flatMap { id in parts.first { $0.id == id } ?? allParts.first { $0.id == id } } }

    private func reloadSetLines() {
        guard let db = database, let id = selectedSetID else { setLines = []; setTotals = (0, 0, 0); return }
        let raw = db.setLines(setID: id, includeSpares: showSpares, includeMinifigParts: showMinifigParts, mergeColors: false)
        setTotals = db.partsTotals(raw)
        setLines = mergeColors ? ExplorerDatabase.mergeColours(raw) : raw
    }

    private func reloadPartDetail() {
        guard let db = database, let id = selectedPartID else { colorLines = []; partSetLines = []; return }
        colorLines = db.colorLines(partID: id)
        partSetLines = db.setLines(partID: id)
    }

    /// "Show set" from a part's set list: jump to the Sets tab on that set.
    func showSet(setID: Int) {
        guard let s = allSets.first(where: { $0.id == setID }) else { return }
        resetSetFilters()
        setNumQuery = s.setNum
        selectedSetID = s.id
        mode = .sets
    }

    func showPart(partID: Int) {
        guard let p = allParts.first(where: { $0.id == partID }) else { return }
        resetPartFilters()
        partNumQuery = p.partNum
        selectedPartID = p.id
        mode = .parts
    }

    // MARK: Update tables (Rebrickable CSV downloads, no API key)

    func updateTables() async {
        guard !updating else { return }
        updating = true; updateResult = nil; updateProgress = 0; updateMessage = "Starting …"
        let fm = FileManager.default
        let work = DataLocation.userDirectory.appendingPathComponent("update-\(UUID().uuidString.prefix(8))", isDirectory: true)
        let csvDir = work.appendingPathComponent("csv", isDirectory: true)
        let newDB = work.appendingPathComponent("lego_explorer.sqlite")
        do {
            try fm.createDirectory(at: csvDir, withIntermediateDirectories: true)
            let builder = TableBuilder()
            let report: TableBuilder.Progress = { [weak self] msg, p in
                Task { @MainActor in self?.updateMessage = msg; self?.updateProgress = p }
            }
            try await builder.download(to: csvDir, progress: report)
            try await Task.detached(priority: .userInitiated) {
                try builder.build(csvDir: csvDir, output: newDB, source: "cdn.rebrickable.com (downloaded)", progress: report)
            }.value
            // swap in the new database
            database?.close(); database = nil
            let dest = DataLocation.userDatabase
            try? fm.removeItem(at: dest)
            try fm.moveItem(at: newDB, to: dest)
            try? fm.removeItem(at: work)
            try open(path: dest.path)
            updateResult = (true, "Tables updated: \(info.nSets.formatted()) sets, \(info.nParts.formatted()) parts.")
        } catch {
            try? fm.removeItem(at: work)
            if database == nil { try? open(path: DataLocation.userDatabase.path) }   // keep the app usable
            updateResult = (false, "Update failed: \(error.localizedDescription)")
        }
        updating = false
    }

    func restoreBundledData() {
        do {
            database?.close(); database = nil
            try DataLocation.restoreBundled()
            try open(path: DataLocation.userDatabase.path)
            updateResult = (true, "Restored the data that came with the app.")
        } catch {
            try? open(path: DataLocation.userDatabase.path)
            updateResult = (false, error.localizedDescription)
        }
    }

    // MARK: CSV export of the current parts list

    func setPartsCSV() -> String {
        func esc(_ s: String) -> String { "\"" + s.replacingOccurrences(of: "\"", with: "\"\"") + "\"" }
        var out = mergeColors
            ? "part_num,part_name,colours,quantity,category,subcategory,part_category\n"
            : "part_num,part_name,color_name,quantity,category,subcategory,part_category,is_spare,minifig\n"
        for l in setLines {
            var f = [esc(l.partNum), esc(l.partName), esc(l.colorName), String(l.quantity),
                     esc(l.category), esc(l.subcategory), esc(l.rbCategory)]
            if !mergeColors { f += [l.isSpare ? "True" : "False", esc(l.minifig)] }
            out += f.joined(separator: ",") + "\n"
        }
        return out
    }
}
