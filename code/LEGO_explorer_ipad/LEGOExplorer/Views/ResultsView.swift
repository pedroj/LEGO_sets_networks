import SwiftUI

struct ResultsView: View {
    @Environment(AppModel.self) private var model

    var body: some View {
        @Bindable var model = model
        Group {
            if let err = model.loadError {
                ContentUnavailableView {
                    Label("No data", systemImage: "externaldrive.badge.exclamationmark")
                } description: { Text(err) } actions: {
                    Button("Open Data") { model.showData = true }
                }
            } else {
                switch model.tab {
                case .sets: SetsList()
                case .parts: PartsList()
                }
            }
        }
        .navigationTitle(model.tab == .sets ? "Sets" : "Parts")
        .navigationBarTitleDisplayMode(.inline)
    }
}

private struct SetsList: View {
    @Environment(AppModel.self) private var model
    var body: some View {
        @Bindable var model = model
        List(model.sets, selection: $model.selectedSetID) { s in
            HStack(spacing: 12) {
                RemoteImage(url: s.imgURL, size: 64)
                VStack(alignment: .leading, spacing: 3) {
                    Text(s.name).font(.headline).lineLimit(2)
                    Text("\(s.setNum) · \(s.year.map(String.init) ?? "—")").font(.subheadline).foregroundStyle(.secondary)
                    Text(s.themeName == s.themeGroup ? s.themeName : "\(s.themeName) · \(s.themeGroup)")
                        .font(.caption).foregroundStyle(.secondary)
                }
                Spacer(minLength: 4)
                Text("\(s.numParts.formatted()) parts").font(.caption.monospacedDigit()).foregroundStyle(.secondary)
            }
            .tag(s.id)
        }
        .listStyle(.plain)
        .safeAreaInset(edge: .top, spacing: 0) {
            Text("\(model.sets.count.formatted()) set\(model.sets.count == 1 ? "" : "s") match. Tap one to see its parts.")
                .font(.footnote).foregroundStyle(.secondary)
                .frame(maxWidth: .infinity, alignment: .leading).padding(.horizontal, 16).padding(.vertical, 6)
                .background(.bar)
        }
        .overlay { if model.searched && model.sets.isEmpty { ContentUnavailableView.search } }
        .task(id: model.setFilter) {
            try? await Task.sleep(for: .milliseconds(250))
            if Task.isCancelled { return }
            await model.runSetSearch()
        }
    }
}

private struct PartsList: View {
    @Environment(AppModel.self) private var model
    var body: some View {
        @Bindable var model = model
        List(model.parts, selection: $model.selectedPartID) { p in
            HStack(spacing: 12) {
                RemoteImage(url: p.imgURL, size: 56)
                VStack(alignment: .leading, spacing: 3) {
                    Text(p.name).font(.headline).lineLimit(2)
                    Text(p.partNum).font(.subheadline).foregroundStyle(.secondary)
                    Text("\(p.category) › \(p.subcategory)").font(.caption).foregroundStyle(.secondary)
                }
                Spacer(minLength: 4)
                VStack(alignment: .trailing, spacing: 2) {
                    Text("\(p.nSets.formatted()) sets")
                    Text("\(p.nColors) colours")
                }
                .font(.caption.monospacedDigit()).foregroundStyle(.secondary)
            }
            .tag(p.id)
        }
        .listStyle(.plain)
        .safeAreaInset(edge: .top, spacing: 0) {
            Text("\(model.parts.count.formatted()) part\(model.parts.count == 1 ? "" : "s") match. Tap one to see its colours and sets.")
                .font(.footnote).foregroundStyle(.secondary)
                .frame(maxWidth: .infinity, alignment: .leading).padding(.horizontal, 16).padding(.vertical, 6)
                .background(.bar)
        }
        .overlay { if model.searched && model.parts.isEmpty { ContentUnavailableView.search } }
        .task(id: model.partFilter) {
            try? await Task.sleep(for: .milliseconds(250))
            if Task.isCancelled { return }
            await model.runPartSearch()
        }
    }
}
