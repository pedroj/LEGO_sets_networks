import Foundation

/// Incremental RFC-4180 CSV parser: feed it byte chunks, get rows through `onRow`.
final class CSVParser {
    private enum State { case unquoted, quoted, quotedMaybeEnd }
    private var state = State.unquoted
    private var field: [UInt8] = []
    private var row: [String] = []
    private var rowHasContent = false
    private let onRow: ([String]) throws -> Void

    init(onRow: @escaping ([String]) throws -> Void) { self.onRow = onRow }

    private func endField() {
        row.append(String(decoding: field, as: UTF8.self))
        field.removeAll(keepingCapacity: true)
        rowHasContent = true
    }
    private func endRow() throws {
        if rowHasContent || !field.isEmpty {
            endField()
            let r = row
            row.removeAll(keepingCapacity: true)
            rowHasContent = false
            try onRow(r)
        }
    }

    func feed(_ buf: UnsafeBufferPointer<UInt8>) throws {
        for c in buf {
            switch state {
            case .unquoted:
                switch c {
                case 0x22 where field.isEmpty: state = .quoted
                case 0x2C: endField()
                case 0x0A: try endRow()
                case 0x0D: break
                default: field.append(c)
                }
            case .quoted:
                if c == 0x22 { state = .quotedMaybeEnd } else { field.append(c) }
            case .quotedMaybeEnd:
                switch c {
                case 0x22: field.append(0x22); state = .quoted
                case 0x2C: endField(); state = .unquoted
                case 0x0A: state = .unquoted; try endRow()
                case 0x0D: break
                default: field.append(c); state = .unquoted
                }
            }
        }
    }
    func finish() throws { try endRow() }

    /// Parses a `.csv` or `.csv.gz` file.
    static func parse(file url: URL, onRow: @escaping ([String]) throws -> Void) throws {
        let p = CSVParser(onRow: onRow)
        if url.pathExtension == "gz" {
            try Gzip.forEachChunk(of: url) { try p.feed($0) }
        } else {
            let fh = try FileHandle(forReadingFrom: url)
            defer { try? fh.close() }
            while let d = try fh.read(upToCount: 1 << 18), !d.isEmpty {
                try d.withUnsafeBytes { try p.feed($0.bindMemory(to: UInt8.self)) }
            }
        }
        try p.finish()
    }
}
