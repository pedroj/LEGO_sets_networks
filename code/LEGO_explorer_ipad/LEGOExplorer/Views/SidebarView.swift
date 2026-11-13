import SwiftUI

struct SidebarView: View {
    @Environment(AppModel.self) private var model

    var body: some View {
        @Bindable var model = model
        Form {
            switch model.tab {
            case .sets: SetFilters()
            case .parts: PartFilters()
            }
        }
        .safeAreaInset(edge: .top, spacing: 0) {
            Picker("", selection: $model.tab) {
                ForEach(AppModel.Tab.allCases) { Text($0.rawValue).tag($0) }
            }
            .pickerStyle(.segmented)
            .padding(.horizontal, 16).padding(.vertical, 8)
            .background(.bar)
        }
        .navigationTitle("Filters")
        .toolbar {
            ToolbarItem(placement: .primaryAction) {
                Button { model.showData = true } label: { Label("Data", systemImage: "externaldrive.badge.icloud") }
            }
        }
    }
}

private struct SetFilters: View {
    @Environment(AppModel.self) private var model
    var body: some View {
        @Bindable var model = model
        let bounds = model.lookups.yearRange
        Section("Find sets") {
            FilterField(title: "Set number (e.g. 75144 or 75144-1)", text: $model.setFilter.setNum, keyboard: .numbersAndPunctuation)
            FilterField(title: "Set name contains (e.g. Snowspeeder)", text: $model.setFilter.name)
            Picker("Theme group", selection: $model.setFilter.themeGroup) {
                Text("All").tag("All")
                ForEach(model.lookups.themeGroups, id: \.self) { Text($0).tag($0) }
            }
            .onChange(of: model.setFilter.themeGroup) { _, _ in
                if !model.themeChoices.contains(model.setFilter.theme) { model.setFilter.theme = "All" }
            }
            Picker("Theme", selection: $model.setFilter.theme) {
                ForEach(model.themeChoices, id: \.self) { Text($0).tag($0) }
            }
            Stepper(value: $model.setFilter.yearFrom, in: bounds.lowerBound...max(bounds.lowerBound, model.setFilter.yearTo)) {
                LabeledContent("Year from", value: String(model.setFilter.yearFrom))
            }
            Stepper(value: $model.setFilter.yearTo, in: min(bounds.upperBound, model.setFilter.yearFrom)...bounds.upperBound) {
                LabeledContent("Year to", value: String(model.setFilter.yearTo))
            }
        }
        Section("Parts list options") {
            Toggle("Combine colours of the same part", isOn: $model.setOptions.mergeColors)
            Toggle("Include spare parts", isOn: $model.setOptions.showSpares)
            Toggle("Include minifigure parts", isOn: $model.setOptions.showMinifigParts)
        }
        Section {
            Button("Clear filters", systemImage: "eraser") { model.clearSetFilters() }
        }
    }
}

private struct PartFilters: View {
    @Environment(AppModel.self) private var model
    var body: some View {
        @Bindable var model = model
        Section("Find parts") {
            FilterField(title: "Part number (e.g. 3001 or 3626)", text: $model.partFilter.partNum, keyboard: .numbersAndPunctuation)
            FilterField(title: "Part name contains (e.g. 1 x 2)", text: $model.partFilter.name)
            Picker("Category", selection: $model.partFilter.category) {
                Text("All").tag("All")
                ForEach(model.lookups.categories, id: \.self) { Text($0).tag($0) }
            }
            .onChange(of: model.partFilter.category) { _, _ in
                if !model.subcategoryChoices.contains(model.partFilter.subcategory) { model.partFilter.subcategory = "All" }
            }
            Picker("Subcategory", selection: $model.partFilter.subcategory) {
                ForEach(model.subcategoryChoices, id: \.self) { Text($0).tag($0) }
            }
            Picker("Rebrickable category", selection: $model.partFilter.rbCategory) {
                Text("All").tag("All")
                ForEach(model.lookups.rbCategories, id: \.self) { Text($0).tag($0) }
            }
            Toggle("Only parts found in sets", isOn: $model.partFilter.onlyInSets)
        }
        Section {
            Button("Clear filters", systemImage: "eraser") { model.clearPartFilters() }
        }
    }
}
