import SwiftUI
import ExplorerCore

struct SetDetailView: View {
    @EnvironmentObject var model: AppModel
    let set: SetRecord
    @State private var exporting = false
    @State private var sortOrder = [KeyPathComparator(\PartLine.category)]

    var body: some View {
        VStack(alignment: .leading, spacing: 0) {
            header
            Divider()
            if model.setLines.isEmpty {
                EmptyDetail(symbol: "tray", title: "No parts inventory", message: "This set has no parts in the current selection.")
            } else if model.mergeColors {
                mergedTable
            } else {
                fullTable
            }
        }
        .fileExporter(isPresented: $exporting, document: CSVDocument(text: model.setPartsCSV()),
                      contentType: .commaSeparatedText, defaultFilename: "\(set.setNum)_parts") { _ in }
    }

    private var header: some View {
        HStack(alignment: .top, spacing: 18) {
            RemoteImage(url: set.imageURL, width: 230, height: 170)
                .padding(4).background(.white).overlay(Rectangle().stroke(.quaternary))
            VStack(alignment: .leading, spacing: 5) {
                Text("\(set.setNum) — \(set.name)").font(.title2.weight(.semibold)).textSelection(.enabled)
                Text("\(set.year.map(String.init) ?? "–") · \(set.themeName)" + (set.themeGroup != set.themeName && !set.themeGroup.isEmpty ? " (\(set.themeGroup))" : ""))
                Text("Parts listed by LEGO: \(set.numParts.formatted())").foregroundStyle(.secondary)
                Text("In this list: \(model.setTotals.types.formatted()) part types, \(model.setTotals.colours) colours, \(model.setTotals.pieces.formatted()) pieces")
                    .foregroundStyle(.secondary)
                HStack {
                    Link("Open on Rebrickable", destination: URL(string: "https://rebrickable.com/sets/\(set.setNum)/")!)
                    Button { exporting = true } label: { Label("Download parts list (CSV)", systemImage: "square.and.arrow.down") }
                        .controlSize(.small).disabled(model.setLines.isEmpty)
                }.padding(.top, 2)
            }
            Spacer(minLength: 0)
        }
        .padding(16)
    }

    private var sortedLines: [PartLine] { model.setLines.sorted(using: sortOrder) }

    private var fullTable: some View {
        Table(sortedLines, sortOrder: $sortOrder) {
            TableColumn("Image") { RemoteImage(url: $0.imageURL, width: 48, height: 36) }.width(56)
            TableColumn("Part", value: \.partNum) { Text($0.partNum).textSelection(.enabled) }.width(min: 60, ideal: 80)
            TableColumn("Name", value: \.partName) { Text($0.partName).lineLimit(2) }.width(min: 140, ideal: 220)
            TableColumn("Colour", value: \.colorName) { ColourLabel(rgb: $0.rgb, name: $0.colorName) }.width(min: 100, ideal: 130)
            TableColumn("Qty", value: \.quantity) { Text("\($0.quantity)").monospacedDigit() }.width(40)
            TableColumn("Category", value: \.category) { Text($0.category) }.width(min: 60, ideal: 75)
            TableColumn("Subcategory", value: \.subcategory) { Text($0.subcategory).lineLimit(1) }.width(min: 100, ideal: 140)
            TableColumn("Rebrickable category", value: \.rbCategory) { Text($0.rbCategory).lineLimit(1) }.width(min: 90, ideal: 120)
            TableColumn("Spare") { Text($0.isSpare ? "yes" : "") }.width(44)
            TableColumn("Minifig") { Text($0.minifig).font(.caption) }.width(min: 60, ideal: 80)
        }
    }

    private var mergedTable: some View {
        Table(sortedLines, sortOrder: $sortOrder) {
            TableColumn("Image") { RemoteImage(url: $0.imageURL, width: 48, height: 36) }.width(56)
            TableColumn("Part", value: \.partNum) { Text($0.partNum).textSelection(.enabled) }.width(min: 60, ideal: 80)
            TableColumn("Name", value: \.partName) { Text($0.partName).lineLimit(2) }.width(min: 140, ideal: 220)
            TableColumn("Colours", value: \.colorName) { Text($0.colorName).lineLimit(2) }.width(min: 120, ideal: 200)
            TableColumn("Qty", value: \.quantity) { Text("\($0.quantity)").monospacedDigit() }.width(40)
            TableColumn("Category", value: \.category) { Text($0.category) }.width(min: 60, ideal: 75)
            TableColumn("Subcategory", value: \.subcategory) { Text($0.subcategory).lineLimit(1) }.width(min: 100, ideal: 140)
            TableColumn("Rebrickable category", value: \.rbCategory) { Text($0.rbCategory).lineLimit(1) }.width(min: 90, ideal: 120)
        }
    }
}

struct PartDetailView: View {
    @EnvironmentObject var model: AppModel
    let part: PartRecord
    @State private var colourSort = [KeyPathComparator(\ColorLine.sets, order: .reverse)]
    @State private var setSort: [KeyPathComparator<PartSetLine>] = []

    var body: some View {
        VStack(alignment: .leading, spacing: 0) {
            header
            Divider()
            if model.colorLines.isEmpty {
                EmptyDetail(symbol: "tray", title: "Not in any set", message: "This part does not appear in the set inventories.")
            } else {
                #if os(macOS)
                VSplitView { coloursTable.frame(minHeight: 130, idealHeight: 190); setsTable.frame(minHeight: 160) }
                #else
                VStack(spacing: 0) { coloursTable.frame(height: 190); setsTable }
                #endif
            }
        }
    }

    private var header: some View {
        HStack(alignment: .top, spacing: 18) {
            RemoteImage(url: part.imageURL, width: 150, height: 130)
                .padding(4).background(.white).overlay(Rectangle().stroke(.quaternary))
            VStack(alignment: .leading, spacing: 5) {
                Text("\(part.partNum) — \(part.name)").font(.title2.weight(.semibold)).textSelection(.enabled)
                Text("\(part.category) › \(part.subcategory) · Rebrickable: \(part.rbCategory)")
                Text("Found in \(model.partSetLines.count.formatted()) sets, in \(model.colorLines.count) colours (\(model.colorLines.reduce(0) { $0 + $1.pieces }.formatted()) pieces in total).")
                    .foregroundStyle(.secondary)
                Link("Open on Rebrickable", destination: URL(string: "https://rebrickable.com/parts/\(part.partNum)/")!)
                    .padding(.top, 2)
            }
            Spacer(minLength: 0)
        }
        .padding(16)
    }

    private var coloursTable: some View {
        VStack(alignment: .leading, spacing: 0) {
            Text("Colours of this part").font(.headline).padding(.horizontal, 12).padding(.top, 8).padding(.bottom, 4)
            Table(model.colorLines.sorted(using: colourSort), sortOrder: $colourSort) {
                TableColumn("Image") { RemoteImage(url: $0.imageURL, width: 44, height: 34) }.width(52)
                TableColumn("Colour", value: \.name) { ColourLabel(rgb: $0.rgb, name: $0.name) }
                TableColumn("Sets", value: \.sets) { Text("\($0.sets)").monospacedDigit() }.width(60)
                TableColumn("Pieces", value: \.pieces) { Text($0.pieces.formatted()).monospacedDigit() }.width(70)
            }
        }
    }

    private var setsTable: some View {
        VStack(alignment: .leading, spacing: 0) {
            Text("Sets that contain this part").font(.headline).padding(.horizontal, 12).padding(.top, 8).padding(.bottom, 4)
            Table(sortedSets, sortOrder: $setSort) {
                TableColumn("Image") { RemoteImage(url: $0.imageURL, width: 52, height: 38) }.width(58)
                TableColumn("Set", value: \.setNum) { Text($0.setNum).textSelection(.enabled) }.width(min: 60, ideal: 75)
                TableColumn("Name", value: \.setName) { Text($0.setName).lineLimit(2) }.width(min: 120, ideal: 170)
                TableColumn("Year") { Text($0.year.map(String.init) ?? "–") }.width(50)
                TableColumn("Theme", value: \.themeName) { Text($0.themeName).lineLimit(1) }.width(min: 80, ideal: 100)
                TableColumn("Qty", value: \.quantity) { Text("\($0.quantity)").monospacedDigit() }.width(44)
                TableColumn("Colours") { Text($0.colours).lineLimit(2).font(.caption) }.width(min: 90, ideal: 120)
                TableColumn("") { s in
                    Button { model.showSet(setID: s.setID) } label: { Label("Show set", systemImage: "arrow.right.circle") }
                        .labelStyle(.iconOnly).buttonStyle(.borderless).help("Show this set's parts list")
                }.width(34)
            }
        }
    }

    private var sortedSets: [PartSetLine] { setSort.isEmpty ? model.partSetLines : model.partSetLines.sorted(using: setSort) }
}
