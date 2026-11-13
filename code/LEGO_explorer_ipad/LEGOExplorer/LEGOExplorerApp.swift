import SwiftUI

@main
struct LEGOExplorerApp: App {
    @State private var model = AppModel()

    init() {
        // big picture cache: part and set photos load from Rebrickable once, then stay on the iPad
        URLCache.shared = URLCache(memoryCapacity: 64 << 20, diskCapacity: 600 << 20)
    }

    var body: some Scene {
        WindowGroup {
            RootView()
                .environment(model)
                .task { await model.load() }
        }
    }
}
