import SwiftUI
import UniformTypeIdentifiers

/// A picture bundled with the app (Resources/<name>.png).
func bundledImage(_ name: String) -> Image? {
    guard let url = Bundle.main.url(forResource: name, withExtension: "png") else { return nil }
    #if os(macOS)
    guard let img = NSImage(contentsOf: url) else { return nil }
    return Image(nsImage: img)
    #else
    guard let img = UIImage(contentsOfFile: url.path) else { return nil }
    return Image(uiImage: img)
    #endif
}

/// Rebrickable picture, loaded over the internet (cached on disk), with a neutral placeholder.
struct RemoteImage: View {
    let url: URL?
    var width: CGFloat = 56
    var height: CGFloat = 42

    var body: some View {
        Group {
            if let url {
                AsyncImage(url: url, transaction: Transaction(animation: .easeIn(duration: 0.15))) { phase in
                    switch phase {
                    case .success(let img): img.resizable().scaledToFit()
                    case .failure: placeholder(systemName: "photo")
                    default: placeholder(systemName: nil)
                    }
                }
            } else {
                placeholder(systemName: "square.dashed")
            }
        }
        .frame(width: width, height: height)
    }

    private func placeholder(systemName: String?) -> some View {
        ZStack {
            RoundedRectangle(cornerRadius: 4).fill(.quaternary.opacity(0.5))
            if let systemName { Image(systemName: systemName).foregroundStyle(.tertiary) }
        }
    }
}

/// Colour swatch + colour name, as in the Shiny tables.
struct ColourLabel: View {
    let rgb: String
    let name: String
    var body: some View {
        HStack(spacing: 6) {
            RoundedRectangle(cornerRadius: 2.5)
                .fill(Color(hex: rgb))
                .overlay(RoundedRectangle(cornerRadius: 2.5).stroke(.gray.opacity(0.7), lineWidth: 0.7))
                .frame(width: 13, height: 13)
            Text(name).lineLimit(1)
        }
    }
}

extension Color {
    init(hex: String) {
        var v: UInt64 = 0xFFFFFF
        Scanner(string: hex).scanHexInt64(&v)
        self.init(red: Double((v >> 16) & 0xFF) / 255, green: Double((v >> 8) & 0xFF) / 255, blue: Double(v & 0xFF) / 255)
    }
}

struct FilterPicker: View {
    let title: String
    @Binding var selection: String
    let options: [String]
    var body: some View {
        Picker(title, selection: $selection) {
            ForEach(options, id: \.self) { Text($0).tag($0) }
        }
    }
}

struct CSVDocument: FileDocument {
    static var readableContentTypes: [UTType] { [.commaSeparatedText] }
    var text: String
    init(text: String) { self.text = text }
    init(configuration: ReadConfiguration) throws {
        text = String(decoding: configuration.file.regularFileContents ?? Data(), as: UTF8.self)
    }
    func fileWrapper(configuration: WriteConfiguration) throws -> FileWrapper {
        FileWrapper(regularFileWithContents: Data(text.utf8))
    }
}

struct EmptyDetail: View {
    let symbol: String
    let title: String
    let message: String
    var body: some View {
        ContentUnavailableView(title, systemImage: symbol, description: Text(message))
    }
}
