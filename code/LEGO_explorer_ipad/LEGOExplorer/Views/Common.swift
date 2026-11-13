import SwiftUI

extension Color {
    /// "F2CD37" -> colour (Rebrickable's rgb column)
    init(hex: String?) {
        guard let hex, hex.count == 6, let v = UInt32(hex, radix: 16) else { self = .white; return }
        self.init(red: Double((v >> 16) & 0xFF) / 255, green: Double((v >> 8) & 0xFF) / 255, blue: Double(v & 0xFF) / 255)
    }
}

/// Picture loaded from Rebrickable (cached by URLCache).
struct RemoteImage: View {
    let url: URL?
    var size: CGFloat = 56
    var body: some View {
        AsyncImage(url: url) { phase in
            switch phase {
            case .success(let img): img.resizable().scaledToFit()
            case .failure: Image(systemName: "photo").foregroundStyle(.tertiary)
            default: if url == nil { Image(systemName: "cube").foregroundStyle(.tertiary) } else { ProgressView().controlSize(.small) }
            }
        }
        .frame(width: size, height: size)
        .padding(2)
        .background(Color.white, in: RoundedRectangle(cornerRadius: 6))
        .overlay(RoundedRectangle(cornerRadius: 6).stroke(Color.gray.opacity(0.25)))
    }
}

struct Swatch: View {
    let rgb: String?
    var body: some View {
        RoundedRectangle(cornerRadius: 3).fill(Color(hex: rgb))
            .frame(width: 14, height: 14)
            .overlay(RoundedRectangle(cornerRadius: 3).stroke(Color.gray.opacity(0.6)))
    }
}

/// Text field with a clear button, used by the filters.
struct FilterField: View {
    let title: String
    @Binding var text: String
    var keyboard: UIKeyboardType = .default
    var body: some View {
        HStack {
            TextField(title, text: $text)
                .keyboardType(keyboard)
                .textInputAutocapitalization(.never)
                .autocorrectionDisabled()
                .submitLabel(.search)
            if !text.isEmpty {
                Button { text = "" } label: { Image(systemName: "xmark.circle.fill").foregroundStyle(.secondary) }
                    .buttonStyle(.plain)
            }
        }
    }
}

func csvEscape(_ s: String) -> String {
    s.contains(where: { $0 == "," || $0 == "\"" || $0 == "\n" }) ? "\"" + s.replacingOccurrences(of: "\"", with: "\"\"") + "\"" : s
}
