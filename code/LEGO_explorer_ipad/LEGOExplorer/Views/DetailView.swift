import SwiftUI
import UniformTypeIdentifiers
import CoreTransferable

struct DetailView: View {
    @Environment(AppModel.self) private var model
    var body: some View {
        switch model.tab {
        case .sets:
            if let id = model.selectedSetID { SetDetailView(setID: id) }
            else { ContentUnavailableView("Select a set", systemImage: "shippingbox", description: Text("Pick a set in the list to see its parts.")) }
        case .parts:
            if let id = model.selectedPartID { PartDetailView(partID: id) }
            else { ContentUnavailableView("Select a part", systemImage: "square.grid.3x3", description: Text("Pick a part in the list to see its colours and the sets that contain it.")) }
        }
    }
}

/// CSV export through the share sheet.
struct CSVFile: Transferable {
    let name: String
    let text: String
    static var transferRepresentation: some TransferRepresentation {
        FileRepresentation(exportedContentType: .commaSeparatedText) { f in
            let url = FileManager.default.temporaryDirectory.appendingPathComponent(f.name)
            try Data(f.text.utf8).write(to: url)
            return SentTransferredFile(url)
        }
    }
}

// MARK: - Set

private struct SetKey: Hashable { let id: Int; let options: SetListOptions }

struct SetDetailView: View {
    @Environment(AppModel.self) private var model
    let setID: Int
    @State private var set: SetRow?
    @State private var contents: SetContents?
    @State private var groups: [(String, [PartLine])] = []

    var body: some View {
        List {
            if let set {
                Section { header(set).listRowSeparator(.hidden) }
            }
            if let contents, contents.lines.isEmpty {
                Text("No parts inventory for this set.").foregroundStyle(.secondary)
            }
            ForEach(groups, id: \.0) { cat, lines in
                Section("\(cat) (\(lines.count))") {
                    ForEach(lines) { l in
                        Button { model.openPart(number: l.partNum) } label: { PartLineRow(line: l, merged: model.setOptions.mergeColors) }
                            .buttonStyle(.plain)
                    }
                }
            }
        }
        .listStyle(.insetGrouped)
        .navigationTitle(set.map { "\($0.setNum) — \($0.name)" } ?? "Set")
        .navigationBarTitleDisplayMode(.inline)
        .task(id: SetKey(id: setID, options: model.setOptions)) { await load() }
    }

    private func load() async {
        guard let store = model.store else { return }
        set = try? await store.set(id: setID)
        guard let c = try? await store.contents(setID: setID, options: model.setOptions) else { return }
        contents = c
        var g: [(String, [PartLine])] = []
        for l in c.lines {
            if g.last?.0 == l.category { g[g.count - 1].1.append(l) } else { g.append((l.category, [l])) }
        }
        groups = g
    }

    private func header(_ s: SetRow) -> some View {
        HStack(alignment: .top, spacing: 20) {
            RemoteImage(url: s.imgURL, size: 240)
            VStack(alignment: .leading, spacing: 8) {
                Text("\(s.setNum) — \(s.name)").font(.title2.weight(.semibold))
                Text("\(s.year.map(String.init) ?? "—") · \(s.themeName)\(s.themeGroup != s.themeName ? " (\(s.themeGroup))" : "")")
                Text("Parts listed by LEGO: \(s.numParts.formatted())").foregroundStyle(.secondary)
                if let c = contents {
                    Text("In this list: \(c.partTypes.formatted()) part types, \(c.colours) colours, \(c.pieces.formatted()) pieces")
                        .foregroundStyle(.secondary)
                }
                HStack(spacing: 16) {
                    Link(destination: URL(string: "https://rebrickable.com/sets/\(s.setNum)/")!) {
                        Label("Open on Rebrickable", systemImage: "safari")
                    }
                    if let c = contents, !c.lines.isEmpty {
                        ShareLink(item: csv(for: s, c), preview: SharePreview("\(s.setNum)_parts.csv")) {
                            Label("Parts list (CSV)", systemImage: "square.and.arrow.up")
                        }
                    }
                }
                .padding(.top, 4)
            }
        }
    }

    private func csv(for s: SetRow, _ c: SetContents) -> CSVFile {
        var out = "set_num,part_num,part_name,color,quantity,category,subcategory,rebrickable_category,is_spare,minifig\n"
        for l in c.lines {
            out += [s.setNum, l.partNum, l.name, l.colorName, String(l.quantity), l.category, l.subcategory, l.rbCategory,
                    l.isSpare ? "True" : "False", l.minifig ?? ""].map(csvEscape).joined(separator: ",") + "\n"
        }
        return CSVFile(name: "\(s.setNum)_parts.csv", text: out)
    }
}

private struct PartLineRow: View {
    let line: PartLine
    let merged: Bool
    var body: some View {
        HStack(spacing: 12) {
            RemoteImage(url: line.imgURL, size: 48)
            VStack(alignment: .leading, spacing: 3) {
                Text(line.name).font(.body).lineLimit(2)
                HStack(spacing: 6) {
                    Text(line.partNum).foregroundStyle(.secondary)
                    if !merged { Swatch(rgb: line.rgb) }
                    Text(line.colorName).lineLimit(1)
                }
                .font(.subheadline)
                Text("\(line.subcategory) · \(line.rbCategory)\(line.isSpare ? " · spare" : "")\(line.minifig.map { " · \($0)" } ?? "")")
                    .font(.caption).foregroundStyle(.secondary)
            }
            Spacer()
            Text("× \(line.quantity)").font(.title3.monospacedDigit().weight(.medium))
            Image(systemName: "chevron.right").font(.caption).foregroundStyle(.tertiary)
        }
        .contentShape(Rectangle())
    }
}

// MARK: - Part

struct PartDetailView: View {
    @Environment(AppModel.self) private var model
    let partID: Int
    @State private var detail: PartDetail?

    var body: some View {
        List {
            if let d = detail {
                Section { header(d).listRowSeparator(.hidden) }
                Section("Colours of this part") {
                    ForEach(d.colors) { c in
                        HStack(spacing: 12) {
                            RemoteImage(url: c.imgURL, size: 44)
                            Swatch(rgb: c.rgb)
                            Text(c.name)
                            Spacer()
                            Text("\(c.sets.formatted()) sets · \(c.pieces.formatted()) pieces")
                                .font(.subheadline.monospacedDigit()).foregroundStyle(.secondary)
                        }
                    }
                }
                Section("Sets that contain this part (\(d.sets.count.formatted()))") {
                    ForEach(d.sets) { u in
                        Button { model.openSet(number: u.set.setNum) } label: {
                            HStack(spacing: 12) {
                                RemoteImage(url: u.set.imgURL, size: 48)
                                VStack(alignment: .leading, spacing: 3) {
                                    Text(u.set.name).lineLimit(2)
                                    Text("\(u.set.setNum) · \(u.set.year.map(String.init) ?? "—") · \(u.set.themeName)")
                                        .font(.subheadline).foregroundStyle(.secondary)
                                    Text(u.colours).font(.caption).foregroundStyle(.secondary).lineLimit(1)
                                }
                                Spacer()
                                Text("× \(u.quantity)").font(.title3.monospacedDigit().weight(.medium))
                                Image(systemName: "chevron.right").font(.caption).foregroundStyle(.tertiary)
                            }
                            .contentShape(Rectangle())
                        }
                        .buttonStyle(.plain)
                    }
                }
            }
        }
        .listStyle(.insetGrouped)
        .navigationTitle(detail.map { "\($0.part.partNum) — \($0.part.name)" } ?? "Part")
        .navigationBarTitleDisplayMode(.inline)
        .task(id: partID) {
            detail = try? await model.store?.detail(partID: partID)
        }
    }

    private func header(_ d: PartDetail) -> some View {
        let p = d.part
        return HStack(alignment: .top, spacing: 20) {
            RemoteImage(url: p.imgURL, size: 200)
            VStack(alignment: .leading, spacing: 8) {
                Text("\(p.partNum) — \(p.name)").font(.title2.weight(.semibold))
                Text("\(p.category) › \(p.subcategory) · Rebrickable: \(p.rbCategory)")
                Text("Found in \(d.sets.count.formatted()) sets, in \(d.colors.count) colours (\(d.totalPieces.formatted()) pieces in total).")
                    .foregroundStyle(.secondary)
                Link(destination: URL(string: "https://rebrickable.com/parts/\(p.partNum)/")!) {
                    Label("Open on Rebrickable", systemImage: "safari")
                }
                .padding(.top, 4)
            }
        }
    }
}
