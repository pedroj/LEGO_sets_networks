import SwiftUI
import ExplorerCore

@main
struct LEGOExplorerApp: App {
    @StateObject private var model = AppModel()

    init() {
        // Part and set pictures load from Rebrickable; keep them cached on disk.
        URLCache.shared = URLCache(memoryCapacity: 64 << 20, diskCapacity: 512 << 20)
    }

    var body: some Scene {
        WindowGroup("LEGO Sets & Parts Explorer") {
            RootView()
                .environmentObject(model)
                .tint(Theme.blue)
                .task { model.load() }
                #if os(macOS)
                .frame(minWidth: 1000, minHeight: 640)
                #endif
        }
        #if os(macOS)
        .defaultSize(width: 1380, height: 860)
        #endif
        .commands {
            CommandGroup(after: .newItem) {
                Button("Update Tables…") { Task { await model.updateTables() } }
                    .keyboardShortcut("u", modifiers: [.command, .shift])
                    .disabled(model.updating)
            }
        }
    }
}

enum Theme {
    static let blue = Color(red: 0, green: 0x55 / 255, blue: 0xBF / 255)     // LEGO blue, as in the Shiny app
}
