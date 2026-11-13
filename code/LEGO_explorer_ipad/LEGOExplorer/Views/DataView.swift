import SwiftUI

/// The "Data" tab of the web app: what the tables contain, and the Update button.
struct DataView: View {
    @Environment(AppModel.self) private var model
    @Environment(\.dismiss) private var dismiss
    @State private var confirmRevert = false

    var body: some View {
        NavigationStack {
            Form {
                Section("Data tables") {
                    let i = model.lookups.info
                    if i.built.isEmpty {
                        Text("No tables loaded yet.").foregroundStyle(.secondary)
                    } else {
                        LabeledContent("Built", value: i.built)
                        LabeledContent("Source", value: i.source)
                        LabeledContent("Sets", value: i.nSets.formatted())
                        LabeledContent("Parts", value: i.nParts.formatted())
                        LabeledContent("Set-part rows", value: i.nRows.formatted())
                        LabeledContent("Minifigure parts included", value: i.minifigParts ? "yes" : "no")
                        LabeledContent("Using", value: DataLocation.hasCustom ? "tables updated on this iPad" : "tables bundled with the app")
                    }
                }
                Section {
                    Text("Downloads the latest Rebrickable tables (no API key needed) and rebuilds the database on this iPad. It needs a network connection, takes a couple of minutes, and uses about 100 MB of storage. Searching stays offline.")
                        .font(.footnote).foregroundStyle(.secondary)
                    Button { Task { await model.updateTables() } } label: { Label("Update tables now", systemImage: "arrow.triangle.2.circlepath") }
                        .disabled(model.isUpdating)
                    switch model.update {
                    case .running(let msg, let f):
                        VStack(alignment: .leading, spacing: 6) {
                            ProgressView(value: f)
                            Text(msg).font(.footnote).foregroundStyle(.secondary)
                        }
                    case .finished(let m): Label(m, systemImage: "checkmark.circle.fill").foregroundStyle(.green)
                    case .failed(let m): Label("Update failed: \(m)", systemImage: "exclamationmark.triangle.fill").foregroundStyle(.red)
                    case .idle: EmptyView()
                    }
                    if DataLocation.hasCustom && DataLocation.bundledDB != nil {
                        Button("Go back to the tables bundled with the app", role: .destructive) { confirmRevert = true }
                            .disabled(model.isUpdating)
                    }
                } header: { Text("Update the tables") }
                Section("About") {
                    Text("Data from Rebrickable's public downloads. Pictures load from Rebrickable.")
                    Link("Designed by PedroJ @ Santa Ana Bricks", destination: URL(string: "https://rebrickable.com/users/PedroJ/")!)
                }
            }
            .navigationTitle("Data")
            .navigationBarTitleDisplayMode(.inline)
            .toolbar { ToolbarItem(placement: .confirmationAction) { Button("Done") { dismiss() } } }
            .confirmationDialog("Discard the updated tables?", isPresented: $confirmRevert, titleVisibility: .visible) {
                Button("Use bundled tables", role: .destructive) { Task { await model.revertToBundled() } }
            }
        }
        .interactiveDismissDisabled(model.isUpdating)
    }
}
