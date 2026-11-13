import Foundation
import Compression

/// Streaming gunzip using Apple's Compression framework (raw DEFLATE after the gzip header).
public enum Gzip {
    public static func stream(file: URL, chunk: (UnsafeBufferPointer<UInt8>) throws -> Void) throws {
        let fh = try FileHandle(forReadingFrom: file)
        defer { try? fh.close() }

        // --- gzip header (RFC 1952) ---
        guard var first = try fh.read(upToCount: 1 << 16), first.count >= 18, first[0] == 0x1f, first[1] == 0x8b, first[2] == 8 else {
            throw SQLiteError(message: "\(file.lastPathComponent) is not a gzip file")
        }
        let flg = first[3]
        var pos = 10
        if flg & 4 != 0 { pos += 2 + Int(first[pos]) + (Int(first[pos + 1]) << 8) }
        if flg & 8 != 0 { while first[pos] != 0 { pos += 1 }; pos += 1 }
        if flg & 16 != 0 { while first[pos] != 0 { pos += 1 }; pos += 1 }
        if flg & 2 != 0 { pos += 2 }
        first = first.subdata(in: pos..<first.count)

        // --- inflate ---
        let dummy = UnsafeMutablePointer<UInt8>.allocate(capacity: 1)
        let dstCap = 1 << 18
        let dst = UnsafeMutablePointer<UInt8>.allocate(capacity: dstCap)
        defer { dummy.deallocate(); dst.deallocate() }
        var stream = compression_stream(dst_ptr: dummy, dst_size: 0, src_ptr: UnsafePointer(dummy), src_size: 0, state: nil)
        guard compression_stream_init(&stream, COMPRESSION_STREAM_DECODE, COMPRESSION_ZLIB) == COMPRESSION_STATUS_OK else {
            throw SQLiteError(message: "cannot start decompression")
        }
        defer { compression_stream_destroy(&stream) }
        stream.dst_ptr = dst; stream.dst_size = dstCap

        var finished = false
        func run(_ data: Data, final: Bool) throws {
            try data.withUnsafeBytes { (raw: UnsafeRawBufferPointer) in
                let base = raw.bindMemory(to: UInt8.self).baseAddress ?? UnsafePointer(dummy)
                stream.src_ptr = base; stream.src_size = data.count
                var idle = 0
                while !finished {
                    let status = compression_stream_process(&stream, final ? Int32(COMPRESSION_STREAM_FINALIZE.rawValue) : 0)
                    if status == COMPRESSION_STATUS_ERROR { throw SQLiteError(message: "corrupt gzip data") }
                    let produced = dstCap - stream.dst_size
                    if produced > 0 { try chunk(UnsafeBufferPointer(start: dst, count: produced)) }
                    stream.dst_ptr = dst; stream.dst_size = dstCap
                    if status == COMPRESSION_STATUS_END { finished = true; break }
                    if final { idle = produced == 0 ? idle + 1 : 0; if idle > 3 { break } }   // keep flushing until END
                    else if stream.src_size == 0 && produced < dstCap { break }
                }
            }
        }
        try run(first, final: false)
        while !finished, let d = try fh.read(upToCount: 1 << 18), !d.isEmpty { try run(d, final: false) }
        if !finished { try run(Data(), final: true) }
    }
}

/// Streaming CSV reader (RFC 4180 quoting). Rows are delivered as arrays of strings.
public final class CSVReader {
    private var field: [UInt8] = []
    private var row: [String] = []
    private var inQuotes = false
    private var quoteSeen = false      // inside quotes, just saw a '"' (escaped quote or closing quote?)
    private let onRow: ([String]) throws -> Void

    public init(onRow: @escaping ([String]) throws -> Void) { self.onRow = onRow }

    private func endField() {
        row.append(String(decoding: field, as: UTF8.self))
        field.removeAll(keepingCapacity: true)
    }
    private func endRow() throws {
        endField()
        if !(row.count == 1 && row[0].isEmpty) { try onRow(row) }
        row.removeAll(keepingCapacity: true)
    }

    public func feed(_ buf: UnsafeBufferPointer<UInt8>) throws {
        for b in buf {
            if inQuotes {
                if quoteSeen {
                    quoteSeen = false
                    if b == 0x22 { field.append(0x22); continue }       // "" -> literal quote
                    inQuotes = false                                    // the quote closed the field; handle b below
                } else if b == 0x22 { quoteSeen = true; continue }
                else { field.append(b); continue }
            }
            switch b {
            case 0x22 where field.isEmpty: inQuotes = true
            case 0x2c: endField()
            case 0x0a: try endRow()
            case 0x0d: break
            default: field.append(b)
            }
        }
    }

    public func finish() throws {
        if inQuotes { inQuotes = false; quoteSeen = false }
        if !field.isEmpty || !row.isEmpty { try endRow() }
    }
}
