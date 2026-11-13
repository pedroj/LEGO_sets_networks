import Foundation
import Observation
import UIKit

/// Where the database lives: the copy bundled in the app, or a newer one built on the
/// iPad by "Update tables" (kept in Application Support).
enum DataLocation {
    static var supportDir: URL {
        let d = FileManager.default.urls(for: .applicationSupportDirectory, in: .userDomainMask)[0]
        try? FileManager.default.createDirectory(at: d, withIntermediateDirectories: true)
        return d
    }
    static var customDB: URL { supportDir.appendingPathComponent("lego.sqlite") }
    static var bundledDB: URL? { Bundle.main.url(forResource: "lego", withExtension: "sqlite") }
    static var hasCustom: Bool { FileManager.default.fileExists(atPath: customDB.path) }
    static var activePath: String? { hasCustom ? customDB.path : bundledDB?.path }
}

enum UpdateState: Equatable {
    case idle
    case running(String, Double)
    case finished(String)
    case failed(String)
}

@MainActor @Observable
final class AppModel {
    enum Tab: String, CaseIterable, Identifiable {
        case sets = "Sets", parts = "Parts"
        var id: String { rawValue }
    }

    var tab: Tab = .sets
    private(set) var store: LegoStore?
    private(set) var lookups = Lookups()
    var loadError: String?
    var showData = false

    var setFilter = SetFilter()
    var partFilter = PartFilter()
    var setOptions = SetListOptions()
    private(set) var sets: [SetRow] = []
    private(set) var parts: [PartRow] = []
    var selectedSetID: Int?
    var selectedPartID: Int?
    private(set) var searched = false

    var update: UpdateState = .idle

    // MARK: Lookups for the pickers

    var themeChoices: [String] {
        let l = setFilter.themeGroup == "All" ? lookups.allThemes : (lookups.themesByGroup[setFilter.themeGroup] ?? [])
        return ["All"] + l
    }
    var subcategoryChoices: [String] {
        let l = partFilter.category == "All" ? lookups.allSubcategories : (lookups.subcategoriesByCategory[partFilter.category] ?? [])
        return ["All"] + l
    }

    // MARK: Loading

    func load() async {
        guard let path = DataLocation.activePath else {
            loadError = "No data yet. Open Data and press “Update tables” to download the Rebrickable tables."
            return
        }
        do {
            let s = try LegoStore(path: path)
            let l = try await s.lookups()
            store = s
            lookups = l
            loadError = nil
            setFilter.yearFrom = l.yearRange.lowerBound
            setFilter.yearTo = l.yearRange.upperBound
            selectedSetID = nil
            selectedPartID = nil
            await runSetSearch()
            await runPartSearch()
        } catch {
            store = nil
            loadError = "Could not open the database: \(error.localizedDescription)"
        }
    }

    // MARK: Searching

    func runSetSearch() async {
        guard let store else { return }
        let f = setFilter
        guard let rows = try? await store.searchSets(f), f == setFilter else { return }
        sets = rows
        searched = true
        // a set-number search that points to one set selects it straight away
        let q = f.setNum.trimmingCharacters(in: .whitespaces).lowercased()
        if rows.count == 1 { selectedSetID = rows[0].id }
        else if !q.isEmpty, let hit = rows.first(where: { [q, q + "-1"].contains($0.setNum.lowercased()) }) {
            selectedSetID = hit.id
        }
    }

    func runPartSearch() async {
        guard let store else { return }
        let f = partFilter
        guard let rows = try? await store.searchParts(f), f == partFilter else { return }
        parts = rows
        let q = f.partNum.trimmingCharacters(in: .whitespaces).lowercased()
        if rows.count == 1 { selectedPartID = rows[0].id }
        else if !q.isEmpty, let hit = rows.first(where: { $0.partNum.lowercased() == q }) {
            selectedPartID = hit.id
        }
    }

    func clearSetFilters() {
        setFilter = SetFilter(yearFrom: lookups.yearRange.lowerBound, yearTo: lookups.yearRange.upperBound)
    }
    func clearPartFilters() {
        partFilter.partNum = ""; partFilter.name = ""
        partFilter.category = "All"; partFilter.subcategory = "All"; partFilter.rbCategory = "All"
    }

    // MARK: Navigation between the tabs

    /// From a set's parts list: show that part in the Parts tab.
    func openPart(number: String) {
        guard let store else { return }
        Task {
            guard let p = try? await store.part(number: number) else { return }
            partFilter = PartFilter(partNum: p.partNum, onlyInSets: false)
            tab = .parts
            await runPartSearch()
            selectedPartID = p.id
        }
    }

    /// From a part's set list: show that set in the Sets tab.
    func openSet(number: String) {
        guard let store else { return }
        Task {
            guard let s = try? await store.set(number: number) else { return }
            setFilter = SetFilter(setNum: s.setNum, yearFrom: lookups.yearRange.lowerBound, yearTo: lookups.yearRange.upperBound)
            tab = .sets
            await runSetSearch()
            selectedSetID = s.id
        }
    }

    // MARK: Updating the tables

    func updateTables() async {
        guard !isUpdating else { return }
        update = .running("Starting …", 0)
        UIApplication.shared.isIdleTimerDisabled = true
        defer { UIApplication.shared.isIdleTimerDisabled = false }
        do {
            try await UpdateService.update { [weak self] msg, f in
                Task { @MainActor in self?.update = .running(msg, f) }
            }
            await load()
            update = .finished("Tables updated: \(lookups.info.nSets.formatted()) sets, \(lookups.info.nParts.formatted()) parts.")
        } catch {
            update = .failed(error.localizedDescription)
        }
    }

    func revertToBundled() async {
        try? FileManager.default.removeItem(at: DataLocation.customDB)
        update = .idle
        await load()
    }

    var isUpdating: Bool { if case .running = update { true } else { false } }
}

extension SetFilter {
    init(setNum: String = "", yearFrom: Int, yearTo: Int) {
        self.init()
        self.setNum = setNum
        self.yearFrom = yearFrom
        self.yearTo = yearTo
    }
}
