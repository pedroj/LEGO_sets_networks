import SwiftUI
import ExplorerCore

struct RootView: View {
    @EnvironmentObject var model: AppModel
    @State private var showData = false
    @State private var columns = NavigationSplitViewVisibility.all

    var body: some View {
        VStack(spacing: 0) {
            BannerView(showData: $showData)
            Divider()
            if let err = model.loadError {
                EmptyDetail(symbol: "exclamationmark.triangle", title: "Could not open the data", message: err)
            } else {
                NavigationSplitView(columnVisibility: $columns) {
                    SidebarView(showData: $showData)
                        .navigationSplitViewColumnWidth(min: 250, ideal: 290, max: 360)
                } content: {
                    ResultsColumn()
                        .navigationSplitViewColumnWidth(min: 320, ideal: 400, max: 560)
                } detail: {
                    DetailColumn()
                }
                .navigationSplitViewStyle(.balanced)
            }
        }
        .sheet(isPresented: $showData) { DataSheet().environmentObject(model) }
        .onChange(of: model.updating) { _, now in if now { showData = true } }
    }
}

/// Banner: icon + title on the left; Santa Ana Bricks picture and credit link on the right.
struct BannerView: View {
    @Binding var showData: Bool
    var body: some View {
        HStack(spacing: 12) {
            if let icon = bundledImage("icon") { icon.resizable().scaledToFit().frame(height: 48) }
            Text("LEGO Sets & Parts Explorer")
                .font(.system(size: 24, weight: .semibold))
            Spacer()
            VStack(spacing: 2) {
                if let logo = bundledImage("sta_ana_bricks") {
                    Link(destination: URL(string: "https://rebrickable.com/users/PedroJ/")!) {
                        logo.resizable().scaledToFit().frame(height: 50).clipShape(RoundedRectangle(cornerRadius: 3))
                    }.buttonStyle(.plain)
                }
                Link("Designed by PedroJ @ Santa Ana Bricks", destination: URL(string: "https://rebrickable.com/users/PedroJ/")!)
                    .font(.system(size: 11))
                    .foregroundStyle(.secondary)
            }
        }
        .padding(.horizontal, 18)
        .padding(.vertical, 6)
        .frame(minHeight: 72)
        .background(.bar)
    }
}

// MARK: - Left: Sets / Parts tabs and their filters

struct SidebarView: View {
    @EnvironmentObject var model: AppModel
    @Binding var showData: Bool

    var body: some View {
        VStack(spacing: 0) {
            Picker("", selection: $model.mode) {
                ForEach(AppModel.Mode.allCases) { Text($0.rawValue).tag($0) }
            }
            .pickerStyle(.segmented)
            .labelsHidden()
            .padding([.horizontal, .top], 12)
            .padding(.bottom, 4)

            Form {
                if model.mode == .sets { SetFilters() } else { PartFilters() }
            }
            .formStyle(.grouped)

            Divider()
            HStack {
                VStack(alignment: .leading, spacing: 1) {
                    Text("Data built \(model.info.built)").font(.caption2).foregroundStyle(.secondary)
                    Text("\(model.info.nSets.formatted()) sets · \(model.info.nParts.formatted()) parts")
                        .font(.caption2).foregroundStyle(.secondary)
                }
                Spacer()
                Button { showData = true } label: { Label("Data", systemImage: "arrow.triangle.2.circlepath") }
                    .controlSize(.small)
            }
            .padding(10)
        }
    }
}

struct SetFilters: View {
    @EnvironmentObject var model: AppModel
    var body: some View {
        Section("Find sets") {
            TextField("Set number", text: $model.setNumQuery, prompt: Text("e.g. 75144 or 75144-1"))
                .autocorrectionDisabled()
            FilterPicker(title: "Theme group", selection: $model.themeGroup, options: [AppModel.all] + model.themeGroups)
            FilterPicker(title: "Theme", selection: $model.theme, options: model.themeChoices)
            TextField("Set name contains", text: $model.setNameQuery, prompt: Text("e.g. Snowspeeder"))
            Picker("Year from", selection: $model.yearFrom) {
                ForEach(Array(model.yearBounds), id: \.self) { Text(String($0)).tag($0) }
            }
            Picker("Year to", selection: $model.yearTo) {
                ForEach(Array(model.yearBounds), id: \.self) { Text(String($0)).tag($0) }
            }
            Button { model.resetSetFilters() } label: { Label("Clear filters", systemImage: "eraser") }
                .disabled(!model.setFiltersActive)
        }
        Section("Parts list options") {
            Toggle("Combine colours of the same part", isOn: $model.mergeColors)
            Toggle("Include spare parts", isOn: $model.showSpares)
            Toggle("Include minifigure parts", isOn: $model.showMinifigParts)
        }
    }
}

struct PartFilters: View {
    @EnvironmentObject var model: AppModel
    var body: some View {
        Section("Find parts") {
            TextField("Part number", text: $model.partNumQuery, prompt: Text("e.g. 3001 or 3626"))
                .autocorrectionDisabled()
            TextField("Part name contains", text: $model.partNameQuery, prompt: Text("e.g. 1 x 2"))
            FilterPicker(title: "Category", selection: $model.category, options: [AppModel.all] + model.categories)
            FilterPicker(title: "Subcategory", selection: $model.subcategory, options: model.subcategoryChoices)
            FilterPicker(title: "Rebrickable category", selection: $model.rbCategory, options: [AppModel.all] + model.rbCategories)
            Toggle("Only parts found in sets", isOn: $model.onlyInSets)
            Button { model.resetPartFilters() } label: { Label("Clear filters", systemImage: "eraser") }
        }
    }
}

// MARK: - Middle: results

struct ResultsColumn: View {
    @EnvironmentObject var model: AppModel
    var body: some View {
        Group {
            if model.mode == .sets { SetsList() } else { PartsList() }
        }
    }
}

struct SetsList: View {
    @EnvironmentObject var model: AppModel
    var body: some View {
        VStack(spacing: 0) {
            HStack {
                Text("\(model.sets.count.formatted()) set\(model.sets.count == 1 ? "" : "s") match")
                    .font(.callout).foregroundStyle(.secondary)
                Spacer()
                Picker("Sort", selection: $model.setSort) {
                    ForEach(AppModel.SetSort.allCases) { Text($0.rawValue).tag($0) }
                }
                .pickerStyle(.menu).labelsHidden().fixedSize()
            }
            .padding(.horizontal, 12).padding(.vertical, 8)
            Divider()
            List(model.sets, selection: $model.selectedSetID) { s in
                HStack(spacing: 10) {
                    RemoteImage(url: s.imageURL, width: 62, height: 46)
                    VStack(alignment: .leading, spacing: 2) {
                        Text(s.name).lineLimit(2)
                        Text("\(s.setNum) · \(s.year.map(String.init) ?? "–") · \(s.themeName)")
                            .font(.caption).foregroundStyle(.secondary).lineLimit(1)
                    }
                    Spacer(minLength: 4)
                    Text("\(s.numParts.formatted())").font(.callout.monospacedDigit()).foregroundStyle(.secondary)
                }
                .tag(s.id)
                .padding(.vertical, 2)
            }
            .listStyle(.inset)
        }
    }
}

struct PartsList: View {
    @EnvironmentObject var model: AppModel
    var body: some View {
        VStack(spacing: 0) {
            HStack {
                Text("\(model.parts.count.formatted()) part\(model.parts.count == 1 ? "" : "s") match")
                    .font(.callout).foregroundStyle(.secondary)
                Spacer()
                Picker("Sort", selection: $model.partSort) {
                    ForEach(AppModel.PartSort.allCases) { Text($0.rawValue).tag($0) }
                }
                .pickerStyle(.menu).labelsHidden().fixedSize()
            }
            .padding(.horizontal, 12).padding(.vertical, 8)
            Divider()
            List(model.parts, selection: $model.selectedPartID) { p in
                HStack(spacing: 10) {
                    RemoteImage(url: p.imageURL, width: 50, height: 46)
                    VStack(alignment: .leading, spacing: 2) {
                        Text(p.name).lineLimit(2)
                        Text("\(p.partNum) · \(p.category) › \(p.subcategory)")
                            .font(.caption).foregroundStyle(.secondary).lineLimit(1)
                    }
                    Spacer(minLength: 4)
                    VStack(alignment: .trailing, spacing: 1) {
                        Text("\(p.nSets.formatted()) sets").font(.caption.monospacedDigit())
                        Text("\(p.nColors) colours").font(.caption2).foregroundStyle(.secondary)
                    }.foregroundStyle(.secondary)
                }
                .tag(p.id)
                .padding(.vertical, 2)
            }
            .listStyle(.inset)
        }
    }
}

// MARK: - Right: detail

struct DetailColumn: View {
    @EnvironmentObject var model: AppModel
    var body: some View {
        switch model.mode {
        case .sets:
            if let s = model.selectedSet { SetDetailView(set: s) }
            else { EmptyDetail(symbol: "shippingbox", title: "Select a set", message: "Pick a set in the list to see its picture and parts.") }
        case .parts:
            if let p = model.selectedPart { PartDetailView(part: p) }
            else { EmptyDetail(symbol: "puzzlepiece", title: "Select a part", message: "Pick a part in the list to see its colours and the sets that contain it.") }
        }
    }
}

struct DataSheet: View {
    @EnvironmentObject var model: AppModel
    @Environment(\.dismiss) private var dismiss

    var body: some View {
        VStack(alignment: .leading, spacing: 14) {
            Text("Data tables").font(.title2.weight(.semibold))
            VStack(alignment: .leading, spacing: 4) {
                Text("Built: \(model.info.built)")
                Text("Source: \(model.info.source)")
                Text("\(model.info.nSets.formatted()) sets, \(model.info.nParts.formatted()) parts, \(model.info.nRows.formatted()) set-part rows")
                Text("Minifigure parts included: \(model.info.minifigParts ? "yes" : "no")")
            }
            .font(.callout)
            Divider()
            Text("Update the tables").font(.headline)
            Text("Downloads Rebrickable's latest public tables (no API key needed) and rebuilds the database on this device. It takes about a minute and about 40 MB of data. Pictures still load from Rebrickable as you browse.")
                .font(.callout).foregroundStyle(.secondary)
            if model.updating {
                ProgressView(value: model.updateProgress) { Text(model.updateMessage).font(.callout) }
            }
            if let r = model.updateResult {
                Label(r.text, systemImage: r.ok ? "checkmark.circle.fill" : "exclamationmark.triangle.fill")
                    .foregroundStyle(r.ok ? .green : .red).font(.callout)
            }
            HStack {
                Button { Task { await model.updateTables() } } label: { Label("Update tables now", systemImage: "arrow.triangle.2.circlepath") }
                    .buttonStyle(.borderedProminent)
                    .disabled(model.updating)
                Button("Restore data shipped with the app") { model.restoreBundledData() }
                    .disabled(model.updating)
                Spacer()
                Button("Done") { dismiss() }.keyboardShortcut(.defaultAction)
            }
        }
        .padding(22)
        .frame(minWidth: 520, idealWidth: 560)
        .interactiveDismissDisabled(model.updating)
    }
}
