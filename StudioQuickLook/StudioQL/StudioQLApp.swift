import SwiftUI

@main
struct StudioQLApp: App {
    var body: some Scene {
        WindowGroup {
            ContentView()
        }
    }
}

struct ContentView: View {
    var body: some View {
        VStack(spacing: 16) {
            Image(systemName: "cube.fill")
                .font(.system(size: 64))
                .foregroundColor(.blue)
            Text("StudioQL")
                .font(.title)
            Text("QuickLook plugin for BrickLink Studio .io files")
                .foregroundColor(.secondary)
            Text("This app provides QuickLook previews and thumbnails for .io files. Keep it in your Applications folder.")
                .multilineTextAlignment(.center)
                .padding()
        }
        .frame(width: 400, height: 300)
    }
}
